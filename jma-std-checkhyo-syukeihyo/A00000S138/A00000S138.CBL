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
       PROGRAM-ID.             A00000S138.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保険別総括表 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/12/10    @@@@-有村     新規作成
      *  04/07/06    @@@@-本橋     保険者、公費情報領域50 → 100に変更
      *  04/08/20    @@@@-本橋     食事療養(日数、請求額、標準負担額)を追加する
      *                            ※別フォームでの印字
      *  04/09/17    @@@@-本橋     MONPE化
      *  05/03/08    @@@@-鈴木     open-cobol 0.23への対応
      *  05/04/12    @@@@-本橋     窓口領収額の集計における不具合修正
      *  05/04/19    @@@@-本橋     船員保険（職務上）が正しく集計されない不具合修正
      *  05/09/07    @@@@-本橋     公費助成額の算出を修正
      *  06/08/09    @@@@-高嶋     sarge対応
      *  07/05/08    @@@@-内田     テスト患者の判定修正
      *  07/08/24    @@@@-大竹     高齢者・老人の給付割合の項目を７割に変更
      *                            食事療養欄の日数を回数に変更
      *  07/09/20    @@@@-長屋     4.0対応
      *  08/04/23    @@@@-田中     H20年4月改正対応(後期高齢者、義務教育就学前)
      *                            旧総合病院の時に診療科が表示されない不具合を修正
      *  08/05/12    @@@@-田中     返戻が正しく印字されない不具合を修正
      *  08/05/26    @@@@-田中     古い公費名を印字してしまう不具合を修正
      *  08/06/25    @@@@-田中     保険者、公費情報領域100 → 300に変更
      *  08/10/14    @@@@-田中     主科設定時に診療科を印字する
      *  08/10/30    @@@@-田中     窓口請求額の不具合を修正
      *  08/12/12    @@@@-門間     自費(095XXXXX、096XXXXXで入力)の集計欄を追加
      *                            食事療養の収益を医療収益に加算
      *  08/12/26    @@@@-門間     医療収益の計算式を変更
      *  09/01/08    @@@@-田中     公費請求額の計算式を変更
      *  09/06/26    @@@@-門間     自費分が正しく集計されない不具合を修正
      *  09/07/01    @@@@-田中     公費請求額の端数処理を修正
      ********************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    国保用ファイル
           SELECT  S138TMP-FILE    ASSIGN    S138TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  S138TMP-KEY
                                   FILE    STATUS  IS  STS-S138TMP.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    国保一時ファイル
       FD  S138TMP-FILE.
           COPY    "A00000S138TMP.INC".
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
      *    国保一時ファイル 名称領域
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //S138TP//.
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //S138TP//.
      ***07/09/20 nagaya 4.0対応 end
      *
           COPY    "A00000S138G1.INC".
           COPY    "A00000S138G2.INC".
           COPY    "A00000S138N1.INC".
           COPY    "A00000S138N2.INC".
      *    
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                 PIC X(02).
           03  STS-RECE                 PIC X(02).
           03  STS-RECEERR              PIC X(02).
           03  STS-RECEUPD              PIC X(02).
           03  STS-S138TMP              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI              PIC 9(01).
           03  FLG-SYSKANRI3             PIC 9(01).
           03  FLG-JYURRK                PIC 9(01).
           03  FLG-PASS                  PIC 9(01).
           03  FLG-TOKUTEI               PIC 9(01).
           03  FLG-TYPE                  PIC 9(01).
           03  FLG-PTINF                 PIC 9(01).
           03  FLG-HKNJAINF              PIC 9(01).
           03  FLG-SYUNOU                PIC 9(01).
           03  FLG-TENSU                 PIC 9(01).
           03  FLG-S138TMP               PIC 9(01).
           03  FLG-PRINT                 PIC 9(01).
           03  INP-STS                   PIC 9(04).
           03  FLG-KUMIAI                PIC 9(01).
           03  FLG-HKNNUM                PIC 9(01).
           03  FLG-PTHKNINF              PIC 9(01).
           03  FLG-HEIYOU                PIC 9(01).
           03  FLG-RJNHKN                PIC 9(01).
           03  FLG-END                   PIC 9(01).
           03  FLG-HKNCOMBI              PIC 9(01).
      *--- 2009/06/26 ADD START ----------------------------------------
           03  FLG-PTHKNINF              PIC 9(01).
      *--- 2009/06/26 ADD END   ----------------------------------------
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                  PIC 9(02).
           03  CNT-PAGE                  PIC 9(06).
           03  CNT-KOHI                  PIC 9(01).
           03  CNT-NINZU                 PIC 9(06).
           03  CNT-JYURRK                PIC 9(06).
           03  CNT-DATA                  PIC 9(06).
           03  CNT-READ                  PIC 9(06).
           03  CNT-KOH                   PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                PIC 9(02).
               05  SYS-MM                PIC 9(02).
               05  SYS-DD                PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                       PIC 9(01).
           03  IDX1                      PIC 9(04).
           03  IDX2                      PIC 9(04).
           03  IDX3                      PIC 9(04).
           03  IDX4                      PIC 9(04).
           03  IDX5                      PIC 9(04).
           03  IDX6                      PIC 9(03).
           03  IDA-G.
               05  IDA                   PIC 9(02).
           03  IDB-G.
               05  IDB                   PIC 9(02).
           03  IDC-G.
               05  IDC                   PIC 9(04).
           03  IDD-G.
               05  IDD                   PIC 9(04).
           03  IDY                       PIC 9(04).
           03  IDZ                       PIC 9(04).
           03  IDM                       PIC 9(04).
           03  IDX-MAX                   PIC 9(04).
           
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID            PIC 9(07).
           03  WRK-PARA-SHELLID          PIC X(08).
      ***07/09/20 nagaya 4.0対応 start
      *     03  WRK-PARA-HOSPID       PIC X(24).
           03  WRK-PARA-HOSPNUM          PIC 9(02).
      ***07/09/20 nagaya 4.0対応 end
      *    対象診療年月
           03  WRK-PARA-SRYYM            PIC X(06).
      *    対象年月最終日・初日
           03  WRK-PARA-PRTLYMD          PIC X(08).
           03  WRK-PARA-PRTSYMD          PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY             PIC 9(04).
               05  WRK-SYSMM             PIC 9(02).
               05  WRK-SYSDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY               PIC 9(04).
               05  WRK-SMM               PIC 9(02).
               05  WRK-SDD               PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY             PIC 9(04).
               05  WRK-SRYMM             PIC 9(02).
               05  WRK-SRYDD             PIC 9(02).
           03  WRK-HENYMDG               PIC X(22).
           03  WRK-SRYYM                 PIC X(22).
           03  WRK-SEIYMD                PIC X(22).
           03  WRK-RECEERR               PIC X(200).
           03  WRK-SRYKA                 PIC X(02).
           03  WRK-NYUGAIKBN             PIC X(01).
           03  WRK-PARA-NYUGAIKBN        PIC X(01).
           03  WRK-CITYNAME              PIC X(12).
           03  WRK-PTID                  PIC 9(10).
           03  WRK-HKNNUM                PIC X(03).
           03  WRK-KOHNUM                PIC X(03).
           03  WRK-TENTNK                PIC 9(04).
           03  WRK-JIBAITNK              PIC 99.9.
           03  WRK-HKNJANUM              PIC X(06).
           03  WRK-HKNJANAME             PIC X(40).
           03  WRK-KYURATE               PIC 9(02).
           03  WRK-KASAN-TBL.
               05  WRK-KENSU             PIC 9(04).
               05  WRK-NISSU             PIC 9(04).
               05  WRK-TENSU             PIC 9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *         05  WRK-MADOGK            PIC 9(09).
               05  WRK-MADOGK            PIC S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/12) line add start monma ---------------------------
               05  WRK-JIHIGK            PIC 9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
               05  WRK-HKNGK             PIC 9(09).
               05  WRK-KOHGK             PIC 9(09).
               05  WRK-SYUEKI            PIC 9(09).
               05  WRK-SNISSU            PIC 9(04).
               05  WRK-SSEIKYU           PIC 9(09).
               05  WRK-SFTNGK            PIC 9(09).
               05  WRK-KKENSU            PIC 9(04).
               05  WRK-KNISSU            PIC 9(04).
               05  WRK-KTENSU            PIC 9(09).
           03  WRK-KOHI-GOKEI-TBL.
               05  WRK-GKKENSU           PIC 9(04).
               05  WRK-GKNISSU           PIC 9(04).
               05  WRK-GKTENSU           PIC 9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *     03  WRK-MADOSEIKYUGK-G        PIC 9(09).
           03  WRK-MADOSEIKYUGK-G        PIC S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/12) line add start monma ---------------------------
           03  WRK-JIHIGK-G              PIC 9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
           03  WRK-KOHSEIKYUGK           PIC 9(09).
           03  WRK-KOHSEIKYUGK-G         PIC 9(09).
           03  WRK-ZENCOMPFTN            PIC 9(09).
           03  WRK-KOHSKJSEIKYUGK        PIC 9(09).
           03  WRK-PATH                  PIC X(64).
           03  WRK-KOHTMP-KEY.
               05  WRK-KOHTMP-RJNKBN     PIC  X(03).
               05  WRK-KOHTMP-KOHNUM     PIC  X(03).
           03  WRK-CHK-TBL.
               05  WRK-CHK-PTID2         PIC 9(10).
               05  WRK-CHK-SRYKA-TBL OCCURS 20.
                   07  WRK-CHK-PTID      PIC 9(10).
                   07  WRK-CHK-DAY-TBL.
                       09  WRK-CHK-DAY   PIC 9(01) OCCURS 31.
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *     03  WRK-CNT1                 PIC 9(02).
      *     03  WRK-CNT2                 PIC 9(02).
      *     03  WRK-CNT3                 PIC 9(02).
      *     03  WRK-CNT4                 PIC 9(02).
           03  WRK-CNT1                  PIC 9(03).
           03  WRK-CNT2                  PIC 9(03).
           03  WRK-CNT3                  PIC 9(03).
           03  WRK-CNT4                  PIC 9(03).
      * 20080625 保険者、公費情報領域100 → 300に変更END
           03  WRK-KOHNAME-G             PIC X(24).
           03  WRK-KOHNAME-G2            PIC X(24).
      *--- 2009/06/26 ADD START ----------------------------------------
           03  WRK-JIHI                  PIC 9(07).
           03  WRK-TGMONEY               PIC 9(07).
           03  WRK-YY1                   PIC 9(04).
           03  WRK-YY2                   PIC 9(04).
           03  WRK-NENREI                PIC 9(03).
           03  WRK-JIHI-TBL        OCCURS 4.
               05  WRK-JIHI-HKNNUM       PIC X(03).
      *--- 2009/06/26 ADD END   ----------------------------------------
      *
           03  WRK-MAE-INPUT             PIC X(200).
           03  WRK-OUT-INPUT             PIC X(200).
      *
           03  WRK-PAGE                  PIC ZZ9.
           03  WRK-Z3                    PIC ZZ9.
           03  WRK-Z4                    PIC ZZZZ.
           03  WRK-Z5                    PIC Z,ZZZ.
           03  WRK-Z6                    PIC ZZ,ZZ9.
           03  WRK-Z7                    PIC ZZZ,ZZZ.
           03  WRK-Z9                    PIC Z,ZZZ,ZZZ.
           03  WRK-Z92                   PIC ZZZZZZZZZ.
           03  WRK-Z10                   PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z11                   PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13                   PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-Z14                   PIC ZZ,ZZZ,ZZZ,ZZZ.
      *--- (2008/12/26) line add start monma ---------------------------
           03  WRK-Z93                   PIC --------Z.
      *--- (2008/12/26) line add end   monma ---------------------------
      *
      * 2004/03/18  追加
           03  WRK-SYS-BEDSU             PIC 9(04).
           03  WRK-SYS-BEDSUIPN          PIC 9(04).
      * 2004/03/18  追加
      *--- 2009/01/08 ADD START ----------------------------------------
           03  WRK-FTNFLG               PIC 9(01).
           03  WRK-HKNFTN               PIC 9(09).
           03  WRK-KOHFTN               PIC 9(09).
      *--- 2009/01/08 ADD END ------------------------------------------
      *
      *    診療科名退避エリア
       01  SUM-AREA.
           03  SUM-SRYKAMAX              PIC 9(03).
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *     03  SUM-SRYDR-TBL       OCCURS 100.
           03  SUM-SRYDR-TBL       OCCURS 300.
      * 20080625 保険者、公費情報領域100 → 300に変更END
               05  SUM-SRYKA             PIC X(02).
               05  SUM-SRYKANAME         PIC X(24).
      *    入院履歴情報退避エリア
       01  WRK-NYUIN-AREA.
           03  WRK-NYUIN-TBL.
               05  WRK-NYUIN-DAY         PIC 9(01) OCCURS 31.
      *
      *
      *    室料差額退避エリア
       01  WRK-ROOM-SAGAKU-TBL.
           03  WRK-ROOM-MAX              PIC 9(02).
           03  WRK-ROOM-DATA     OCCURS 50.
               05  WRK-ROOM-CD           PIC 9(03).
               05  WRK-ROOM-SGK          PIC 9(05).
      *
      *
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
      *--- 2009/06/26 ADD START ----------------------------------------
      *    主科設定情報 
           COPY    "CPSK2009.INC".
      *--- 2009/06/26 ADD END   ----------------------------------------
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
      *　　保険者情報
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
      *--- 2009/06/26 ADD START ----------------------------------------
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *--- 2009/06/26 ADD END   ----------------------------------------
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
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
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
      *     INITIALIZE                  S138TP
      *     INITIALIZE                  S138
      *
      ***07/09/20 nagaya 4.0対応 start
           MOVE    S100LNK-HOSPNUM    TO   WRK-PARA-HOSPNUM
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
      *    ステップ管理開始処理
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    S100LNK-JOBID      TO   JOB-JOBID
           MOVE    S100LNK-SHELLID    TO   JOB-SHELLID
           MOVE    "A00000S100"       TO   JOB-PGID
           MOVE    "保険別請求チェック表(国保)"
                                      TO   JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    S100LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      ***07/09/20 nagaya 4.0対応 end      *
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
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG               TO   WRK-SEIYMD
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正START tanaka
           MOVE  S100LNK-PRTKANRI-SKYYMD   TO   SPA-SYSYMD
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正END
      *
      ***  20040820  @@@@-MOTOHASI  追加
           MOVE   S100LNK-NYUGAIKBN        TO   WRK-PARA-NYUGAIKBN
      ***  20040820
      *
      *
      *    対象日付処理
           MOVE    S100LNK-YM              TO   WRK-SRYYMD (1:6)
           MOVE    "01"                    TO   WRK-SRYYMD (7:2)
           MOVE    S100LNK-YM              TO   WRK-SYMD (1:6)
           MOVE    "01"                    TO   WRK-SYMD (7:2)
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)       TO   WRK-SRYYM 
           MOVE    S100LNK-YM              TO   WRK-PARA-SRYYM
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    S100LNK-YM              TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD                TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    S100LNK-YM              TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                    TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集^M
           MOVE    SPACE                   TO   SYS-1001-REC
           INITIALIZE                           SYS-1001-REC
           MOVE    "1001"                  TO   SYS-1001-KANRICD
           MOVE    "*"                     TO   SYS-1001-KBNCD
      ***07/09/20 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM             TO   SYS-1001-HOSPNUM
           MOVE    SPA-SYSYMD              TO   SYS-1001-STYUKYMD
                                                SYS-1001-EDYUKYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE    SYS-1001-REC            TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI   =   ZERO
                MOVE    MCPDATA-REC        TO   SYS-1001-REC
      ***07/09/20 nagaya 4.0対応 start
      *          MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
                MOVE    SYS-1001-HOSPNUM   TO   WRK-PARA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
                MOVE    SYS-1001-BEDSU     TO   WRK-SYS-BEDSU
                MOVE    SYS-1001-BEDSUIPN  TO   WRK-SYS-BEDSUIPN
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
           MOVE     SPACE              TO   SYS-4001-REC
           INITIALIZE                       SYS-4001-REC
           MOVE     "4001"             TO   SYS-4001-KANRICD
           MOVE     "*"                TO   SYS-4001-KBNCD
      ***07/09/20 nagaya 4.0対応 start
           MOVE     SPA-HOSPNUM        TO   SYS-4001-HOSPNUM
           MOVE     SPA-SYSYMD         TO   SYS-4001-STYUKYMD
                                            SYS-4001-EDYUKYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE     SYS-4001-REC  TO        MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF   FLG-SYSKANRI        =   ZERO
             MOVE    MCPDATA-REC TO  SYS-4001-REC
           ELSE
             INITIALIZE              SYS-4001-REC
           END-IF
           IF   SYS-4001-TENKBN  =  "1"
             MOVE    11.5              TO   WRK-JIBAITNK
           ELSE
             MOVE    12                TO   WRK-JIBAITNK
           END-IF
      *
      *    診療科情報取得
      ***07/09/20 nagaya 4.0対応 start
             MOVE    SPACE                  TO   SYS-1005-REC
             INITIALIZE                          SYS-1005-REC
      ***07/09/20 nagaya 4.0対応 end
             MOVE    "1005"                 TO   SYS-1005-KANRICD
      ***07/09/20 nagaya 4.0対応 start
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正START tanaka
      *       MOVE    SPA-SRYYMD             TO  SYS-1005-STYUKYMD
      *                                          SYS-1005-EDYUKYMD
             MOVE    SPA-SYSYMD             TO   SYS-1005-STYUKYMD
                                                 SYS-1005-EDYUKYMD
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正END
             MOVE    SPA-HOSPNUM            TO   SYS-1005-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
             MOVE    WRK-PARA-PRTLYMD       TO   WRK-SYMD
             PERFORM   820-SYSKANRI-DBSELECT-SEC
             MOVE     1        TO     IDX1
             PERFORM  UNTIL  FLG-SYSKANRI3 = 1
               IF  FLG-SYSKANRI3  =  ZERO
                 MOVE SYS-1005-KBNCD        TO   SUM-SRYKA(IDX1)
                 MOVE SYS-1005-SRYKANAME1   TO   SUM-SRYKANAME(IDX1)
                 ADD    1            TO   IDX1
               END-IF  
               PERFORM   820-SYSKANRI-DBFETCH-SEC
             END-PERFORM
             SUBTRACT    1      FROM   IDX1
             MOVE    IDX1          TO   SUM-SRYKAMAX
      *=== sarge対応 ADD By Takashima(2006/09/17) start
             PERFORM   820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/17) end
      *
      *
           MOVE    "S138TP"                 TO   S138TP-FILE-ID
           MOVE    S100LNK-PRTKANRI-TERMID  TO   S138TP-TERMID
      ***07/09/20 nagaya 4.0対応 start
           MOVE    WRK-PARA-HOSPNUM         TO   S138TP-HOSPNUM
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
           IF    CNT-DATA    >    ZERO
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
           OPEN     OUTPUT  S138TMP-FILE
           CLOSE            S138TMP-FILE
           OPEN     I-O     S138TMP-FILE
      *--- 2009/06/26 ADD START ----------------------------------------
      *    自費データ集計
           IF  WRK-PARA-NYUGAIKBN  =  2
             OR  WRK-PARA-NYUGAIKBN  =  SPACE
             PERFORM  2001-JIHI-SYUKEI-SEC
           END-IF
      *--- 2009/06/26 ADD END   ----------------------------------------
      *    国保データ集計
           PERFORM  2001-KOKUHO-SYUKEI-SEC
      *
           CLOSE   S138TMP-FILE
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD START ----------------------------------------
      *****************************************************************
      *    自費データ抽出処理１
      *****************************************************************
       2001-JIHI-SYUKEI-SEC                SECTION.
      *
           MOVE    2        TO   WRK-NYUGAIKBN
           PERFORM  920-DBSELECT3-SEC
             PERFORM  UNTIL   FLG-JYURRK   =   1
      *    データ集計
             PERFORM  2001-JIHI-G-SEC
             PERFORM  920-DBFETCH3-SEC
           END-PERFORM
      *
           PERFORM  920-DBCLOSE3-SEC
      *
           .
       2001-JIHI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費データ抽出処理２
      *****************************************************************
        2001-JIHI-G-SEC                SECTION.
      *
      *    収納情報よりデータ抽出
           PERFORM  940-SYUNOU-READ-SEC
           PERFORM  910-PTHKNINF-READ-SEC
           IF  SYU-SYUHKNNUM  =  "060"
             OR  SYU-SYUHKNNUM  =  "067"
             IF       SYU-TOTAL-HKNTEN            =  0
               AND    SYU-TOTAL-MONEY             =  0
               AND  ( SYU-JIHI-TOTAL         NOT  =  0
               OR     SYU-JIHI-TOTAL-TAX     NOT  =  0 
               OR     SYU-TOTAL-TGMONEY      NOT  =  0
               OR     SYU-TOTAL-TGMONEY-TAX  NOT  =  0 )
               COMPUTE  WRK-JIHI     =  WRK-JIHI
                                     +  SYU-JIHI-TOTAL
                                     +  SYU-JIHI-TOTAL-TAX
               COMPUTE  WRK-TGMONEY  =  WRK-TGMONEY
                                     +  SYU-TOTAL-TGMONEY
                                     +  SYU-TOTAL-TGMONEY-TAX
               PERFORM  2001-KASAN-JIHI-G-SEC
               MOVE     0   TO   WRK-JIHI
                                 WRK-TGMONEY
             END-IF
           END-IF
      *
           .
        2001-JIHI-G-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費集計処理１
      *****************************************************************
        2001-KASAN-JIHI-G-SEC               SECTION.
      *
           MOVE     SYU-PTID                TO   WRK-PTID
           PERFORM  900-PTINF-READ-SEC
           PERFORM  930-HKNCOMBI-SET-SEC
      *
      *    保険算定年齢チェック等
           INITIALIZE                       ORCSAGECHKAREA
           MOVE     SYU-HOSPNUM             TO  AGECHK-HOSPNUM
           MOVE     SYU-PTID                TO  AGECHK-PTID
           MOVE     S100LNK-YM              TO  AGECHK-SRYYMD(1:6)
           MOVE     "01"                    TO  AGECHK-SRYYMD(7:2)
           MOVE     COMB-HKNCOMBINUM        TO  AGECHK-HKNCOMBINUM
           CALL     "ORCSAGECHK"            USING
                                            ORCSAGECHKAREA
                                            SPA-AREA
      *
           MOVE     PTINF-BIRTHDAY(1:4)     TO    WRK-YY1
           MOVE     S100LNK-YM(1:4)         TO    WRK-YY2
           COMPUTE  WRK-NENREI   =   WRK-YY2   -   WRK-YY1
           IF  PTINF-BIRTHDAY(5:2)  >  S100LNK-YM(5:2)
               SUBTRACT   1   FROM   WRK-NENREI
           END-IF
      *
      *    キー設定
           INITIALIZE    S138TMP-REC
      *
           MOVE     SYU-NYUGAIKBN       TO    S138TMP-NYUGAIKBN
      *
           IF    SYS-1001-HOSPSBT1  =  1 
             OR  SYS-2009-SYUKA-KBN = 1
             MOVE    SYU-SRYKA        TO    S138TMP-SRYKA
           ELSE
             MOVE    "00"               TO    S138TMP-SRYKA
           END-IF
      *
           MOVE    "9"       TO   S138TMP-SRYYMKBN
      *    TMPファイル読み込み
           MOVE    2                    TO   FLG-TYPE
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    国保組合判定
           MOVE    ZERO     TO    FLG-KUMIAI
           IF    PTHKN-HKNJANUM(7:2)  =  SPACE
             IF    PTHKN-HKNJANUM(3:1)  =  "3"
               MOVE    1    TO    FLG-KUMIAI
             END-IF
             MOVE  PTHKN-HKNJANUM(1:6)    TO   WRK-HKNJANUM
           ELSE
             IF    PTHKN-HKNJANUM(5:1)  =  "3"
               MOVE    1    TO    FLG-KUMIAI
             END-IF
             MOVE  PTHKN-HKNJANUM(3:6)    TO   WRK-HKNJANUM
           END-IF
      *
      *    公費設定
           MOVE   SYU-KOH1HKNNUM   TO   WRK-JIHI-HKNNUM(1)
           MOVE   SYU-KOH2HKNNUM   TO   WRK-JIHI-HKNNUM(2)
           MOVE   SYU-KOH3HKNNUM   TO   WRK-JIHI-HKNNUM(3)
           MOVE   SYU-KOH4HKNNUM   TO   WRK-JIHI-HKNNUM(4)
      *
      *    明細集計場所設定
           IF   FLG-KUMIAI   =   ZERO
      *    公費集計処理 
             IF    SYU-KOH1HKNNUM  NOT = SPACE
               PERFORM  2001-JIHI-IK-DATA-SET-SEC
             END-IF
             PERFORM 2001-JIHI-IDX-SET-SEC
      *    一般国保集計
             PERFORM 2001-JIHI-DATA-SET-SEC
           ELSE
      *    公費集計処理 
             IF    SYU-KOH1HKNNUM  NOT = SPACE
               PERFORM  2001-JIHI-KK-DATA-SET-SEC
             END-IF
             PERFORM 2001-JIHI-IDX-SET-K-SEC
      *    国保組合集計
             PERFORM 2001-JIHI-DATA-SET-K-SEC
           END-IF
      *
      *    書き出し
           IF  FLG-S138TMP   =  1
             ADD      1             TO   CNT-DATA
             WRITE    S138TMP-REC
           ELSE
             REWRITE  S138TMP-REC
           END-IF
      *
           .
        2001-KASAN-JIHI-G-EXT.
           EXIT.
      *****************************************************************
      *    自費集計場所設定（一般国保）
      *****************************************************************
       2001-JIHI-IDX-SET-SEC                SECTION.
      *
           IF    SYU-SYUHKNNUM = "060"
             IF    WRK-NENREI  >= 70
               IF    SYU-PTFTNRATE  =  10
                 MOVE      1         TO    IDX1
               ELSE
                 MOVE      2         TO    IDX1
               END-IF
             ELSE
               IF    AGECHK-SYUGAKUKBN  =  1
                 MOVE      4       TO    IDX1
               ELSE
                 MOVE      3       TO    IDX1
               END-IF
             END-IF
      *    小計集計場所設定
             MOVE          5         TO    IDX2
           ELSE
             IF    WRK-NENREI    >= 70
               IF    SYU-PTFTNRATE  =  10
                 MOVE      6         TO    IDX1
               ELSE
                 MOVE      7         TO    IDX1
               END-IF
             ELSE
               IF    AGECHK-SYUGAKUKBN  =  1
                 MOVE      10      TO    IDX1
               ELSE
                 IF    COMB-HONKZKKBN  =  "1"
                   MOVE    8       TO    IDX1
                 ELSE
                   MOVE    9       TO    IDX1
                 END-IF
               END-IF
             END-IF
      *    小計集計場所設定
             MOVE          11        TO    IDX2
           END-IF
           .
       2001-JIHI-IDX-SET-EXT.
           EXIT.
      *****************************************************************
      *    自費集計処理２（一般国保）
      *****************************************************************
       2001-JIHI-DATA-SET-SEC                SECTION.
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
             ADD    WRK-TGMONEY          TO    S138TMP-MADOGK(IDX1)
                                               S138TMP-MADOGK(IDX2)
                                               S138TMP-GMADOGK
      *
      *    自費計加算
             ADD    WRK-JIHI             TO    S138TMP-JIHIGK(IDX1)
                                               S138TMP-JIHIGK(IDX2)
                                               S138TMP-GJIHIGK
      *
      *    医療収益
             ADD    WRK-JIHI             TO    S138TMP-SYUEKI(IDX1)
                                               S138TMP-SYUEKI(IDX2)
                                               S138TMP-GSYUEKI
             ADD    WRK-TGMONEY          TO    S138TMP-SYUEKI(IDX1)
                                               S138TMP-SYUEKI(IDX2)
                                               S138TMP-GSYUEKI
      *
             MOVE    1             TO    IDC
             MOVE    300           TO    IDX-MAX
             PERFORM    UNTIL   IDC   >    IDX-MAX
               IF   S138TMP-HKNJANUM(IDC)   =  SPACE  OR  WRK-HKNJANUM
                 MOVE    WRK-HKNJANUM  TO   S138TMP-HKNJANUM(IDC)
                 MOVE    ZERO      TO    IDX-MAX
               ELSE
                 ADD     1         TO    IDC
               END-IF     
             END-PERFORM
      *
             IF    IDC    <=  300
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
             ADD    WRK-TGMONEY          TO    S138TMP-HMADOGK(IDC)
      *
      *    自費計加算
             ADD    WRK-JIHI             TO    S138TMP-HJIHIGK(IDC)
      *
      *    医療収益
             ADD    WRK-JIHI             TO    S138TMP-HSYUEKI(IDC)
             ADD    WRK-TGMONEY          TO    S138TMP-HSYUEKI(IDC)
      *
           END-IF
           .
       2001-JIHI-DATA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理（一般国保）
      *****************************************************************
       2001-JIHI-IK-DATA-SET-SEC                SECTION.
      *
           PERFORM VARYING IDX5     FROM    1   BY  1
                   UNTIL   IDX5   >   4
                   OR  WRK-JIHI-TBL(IDX5) =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     300
                      OR       S138TMP-KOH1-KEY (IDX4) = SPACE
               IF  WRK-JIHI-TBL(IDX5)  =  S138TMP-KOHNUM (IDX4)
                   MOVE  IDX4  TO  CNT-KOH
                   MOVE  300    TO  IDX4
               END-IF
             END-PERFORM
      *
             IF  IDX4  < 300
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    公費名
      *    ０９１と０５１を同じ公費として扱う
             IF   WRK-JIHI-TBL(IDX5)  =  "091"
               MOVE  "051"               TO   S138TMP-KOHNUM(CNT-KOH)
             ELSE
               MOVE  WRK-JIHI-TBL(IDX5)  TO   S138TMP-KOHNUM(CNT-KOH)
             END-IF
      *    自費・保険外のみの場合、件数・日数は集計しない
      *
           END-PERFORM
           .
       2001-JIHI-IK-DATA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費集計場所設定（国保組合）
      *****************************************************************
       2001-JIHI-IDX-SET-K-SEC                SECTION.
      *
           IF    COMB-HONKZKKBN    =    "1"
             IF    WRK-NENREI       >=    70
               IF    SYU-PTFTNRATE  =  10
                 MOVE      1         TO    IDX1
               ELSE
                 MOVE      2         TO    IDX1
               END-IF
             ELSE
               MOVE        3         TO    IDX1
             END-IF
           ELSE
             IF    WRK-NENREI       >=    70
               IF    SYU-PTFTNRATE  =  10
                 MOVE      4         TO    IDX1
               ELSE
                 MOVE      5         TO    IDX1
               END-IF
             ELSE
               IF    AGECHK-SYUGAKUKBN  =  1
                 MOVE      7         TO    IDX1
               ELSE
                 MOVE      6         TO    IDX1
               END-IF
             END-IF
           END-IF
      *    小計集計場所設定
             MOVE            8         TO    IDX2
           .
       2001-JIHI-IDX-SET-K-EXT.
           EXIT.
      *****************************************************************
      *    自費集計処理２（国保組合）
      *****************************************************************
       2001-JIHI-DATA-SET-K-SEC                SECTION.
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
           ADD    WRK-TGMONEY          TO    S138TMP-K-MADOGK(IDX1)
                                             S138TMP-K-MADOGK(IDX2)
                                             S138TMP-GMADOGK
      *
      *    自費計加算
           ADD    WRK-JIHI             TO    S138TMP-K-JIHIGK(IDX1)
                                             S138TMP-K-JIHIGK(IDX2)
                                             S138TMP-GJIHIGK
      *
      *    医療収益
           ADD    WRK-JIHI             TO    S138TMP-K-SYUEKI(IDX1)
                                             S138TMP-K-SYUEKI(IDX2)
                                             S138TMP-GSYUEKI
           ADD    WRK-TGMONEY          TO    S138TMP-K-SYUEKI(IDX1)
                                             S138TMP-K-SYUEKI(IDX2)
                                             S138TMP-GSYUEKI
      *
           MOVE    1             TO    IDC
           MOVE    300            TO    IDX-MAX
           PERFORM    UNTIL   IDC   >    IDX-MAX
             IF   S138TMP-K-HKNJANUM(IDC)   =  SPACE  OR  WRK-HKNJANUM
               MOVE    WRK-HKNJANUM  TO   S138TMP-K-HKNJANUM(IDC)
               MOVE    ZERO      TO    IDX-MAX
             ELSE
               ADD     1         TO    IDC
             END-IF   
           END-PERFORM
           IF    IDC    <=  300
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
             ADD    WRK-TGMONEY          TO    S138TMP-K-HMADOGK(IDC)
      *
      *    自費計加算
             ADD    WRK-JIHI             TO    S138TMP-K-HJIHIGK(IDC)
      *
      *    医療収益
             ADD    WRK-JIHI             TO    S138TMP-K-HSYUEKI(IDC)
             ADD    WRK-TGMONEY          TO    S138TMP-K-HSYUEKI(IDC)
      *
           END-IF
           .
       2001-JIHI-DATA-SET-K-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理(国保組合)
      *****************************************************************
       2001-JIHI-KK-DATA-SET-SEC               SECTION.
      *
           PERFORM VARYING IDX5     FROM    1   BY  1
                   UNTIL   IDX5     >       4
                   OR  WRK-JIHI-TBL(IDX5) =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     300
                      OR       S138TMP-KOH2-KEY (IDX4) = SPACE
               IF  WRK-JIHI-TBL(IDX5)  =  S138TMP-K-KOHNUM(IDX4)
                 MOVE    IDX4      TO   CNT-KOH
                 MOVE    300       TO   IDX4
               END-IF
             END-PERFORM
      *
             IF  IDX4  < 300
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    公費名
             MOVE  WRK-JIHI-TBL(IDX5)  TO  S138TMP-K-KOHNUM(CNT-KOH)
      *    自費・保険外のみの場合、件数・日数は集計しない
      *
           END-PERFORM
           .
       2001-JIHI-KK-DATA-SET-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD END   ----------------------------------------
      *****************************************************************
      *    国保データ抽出処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC                SECTION.
      *
      ***07/09/20 nagaya 4.0対応 start
      *     DISPLAY "**************"
      *     INITIALIZE        WRK-PATH
      *     MOVE    "SEIKYU-KEY4"  TO    WRK-PATH
      ***07/09/20 nagaya 4.0対応 end
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *
      *    集計場所設定
             PERFORM   2001-IDX-SET2-SEC
      *
      *    国保データ集計
             IF    FLG-KUMIAI  =  ZERO
               PERFORM   2001-DATA-SET2-SEC
             ELSE
               PERFORM   2001-DATA-SET2-K-SEC
             END-IF
      *
             PERFORM   900-SEIKYU-READ-SEC
           END-PERFORM
      *
           PERFORM   900-SEIKYU-CLOSE-SEC
      *
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保集計場所設定
      *****************************************************************
       2001-IDX-SET2-SEC                SECTION.
      *
      *    国保組合判定
           MOVE    ZERO    TO    FLG-KUMIAI
           IF    RECE10-HKNJANUM(7:2)  =  SPACE
             IF    RECE10-HKNJANUM(3:1)  =  "3"
               MOVE    1    TO    FLG-KUMIAI
             END-IF
             MOVE  RECE10-HKNJANUM(1:6)    TO   WRK-HKNJANUM
           ELSE
             IF    RECE10-HKNJANUM(5:1)  =  "3"
               MOVE    1    TO    FLG-KUMIAI
             END-IF
             MOVE  RECE10-HKNJANUM(3:6)    TO   WRK-HKNJANUM
           END-IF
      *
      *    明細集計場所設定
           IF    FLG-KUMIAI   =   ZERO
      *    一般国保
             IF    RECE10-RJNHKNNUM NOT = SPACE
               IF    RECE10-KYURATE  =  9
                 MOVE          12      TO    IDX1
               ELSE
                 MOVE          13      TO    IDX1
               END-IF
      *    小計集計場所設定
               MOVE            14      TO    IDX2
             ELSE
               IF    RECE10-HKNNUM = "060"
                 IF    RECE10-AGE  >= 70
                   IF    RECE10-KYURATE  =  9
                     MOVE      1       TO    IDX1
                   ELSE
                     MOVE      2       TO    IDX1
                   END-IF
                 ELSE
      * 20080423 H20年4月改正対応START tanaka
                   IF S100LNK-YM < "200804"
      * 20080423 H20年4月改正対応END
                     IF    RECE10-AGE  <  3
                       MOVE      4       TO    IDX1
                     ELSE
                       MOVE      3       TO    IDX1
                     END-IF
      * 20080423 H20年4月改正対応START tanaka
                   ELSE
                     IF    RECE10-FLG(1)  =  1
                       MOVE      4       TO    IDX1
                     ELSE
                       MOVE      3       TO    IDX1
                     END-IF
                   END-IF
      * 20080423 H20年4月改正対応END
                 END-IF
      *    小計集計場所設定
                 MOVE          5       TO    IDX2
               ELSE
                 IF    RECE10-AGE    >= 70
                   IF    RECE10-KYURATE  =  9
                     MOVE      6       TO    IDX1
                   ELSE
                     MOVE      7       TO    IDX1
                   END-IF
                 ELSE
      * 20080423 H20年4月改正対応START tanaka
                   IF S100LNK-YM < "200804"
      * 20080423 H20年4月改正対応END
                     IF    RECE10-AGE  <  3
                       MOVE      10       TO    IDX1
                     ELSE
                       IF    RECE10-HONKZKKBN  =  "1"
                         MOVE    8       TO    IDX1
                       ELSE
                         MOVE    9       TO    IDX1
                       END-IF
                     END-IF
      * 20080423 H20年4月改正対応START tanaka
                   ELSE
                     IF    RECE10-FLG(1)  =  1
                       MOVE      10       TO    IDX1
                     ELSE
                       IF    RECE10-HONKZKKBN  =  "1"
                         MOVE    8       TO    IDX1
                       ELSE
                         MOVE    9       TO    IDX1
                       END-IF
                     END-IF
                   END-IF
      * 20080423 H20年4月改正対応END
                 END-IF
      *    小計集計場所設定
                 MOVE          11      TO    IDX2
               END-IF
             END-IF
           ELSE
      *    国保組合
             IF    RECE10-RJNHKNNUM  NOT  =  SPACE
               IF    RECE10-KYURATE    =    9
                 MOVE          9       TO    IDX1
               ELSE
                 MOVE          10      TO    IDX1
               END-IF
      *    小計集計場所設定
               MOVE            11      TO    IDX2
             ELSE
               IF    RECE10-HONKZKKBN    =    "1"
                 IF    RECE10-AGE       >=    70
                   IF    RECE10-KYURATE  =    9
                     MOVE      1       TO    IDX1
                   ELSE
                     MOVE      2       TO    IDX1
                   END-IF
                 ELSE
                   MOVE        3       TO    IDX1
                 END-IF
               ELSE
                 IF    RECE10-AGE       >=    70
                   IF    RECE10-KYURATE  =    9
                     MOVE      4       TO    IDX1
                   ELSE
                     MOVE      5       TO    IDX1
                   END-IF
                 ELSE
      * 20080423 H20年4月改正対応START tanaka
                   IF S100LNK-YM < "200804"
      * 20080423 H20年4月改正対応END
                     IF    RECE10-AGE  <  3
                       MOVE      7       TO    IDX1
                     ELSE
                       MOVE      6       TO    IDX1
                     END-IF
      * 20080423 H20年4月改正対応START tanaka
                   ELSE
                     IF    RECE10-FLG(1)  =  1
                       MOVE      7       TO    IDX1
                     ELSE
                       MOVE      6       TO    IDX1
                     END-IF
                   END-IF
      * 20080423 H20年4月改正対応END
                 END-IF
               END-IF
      *    小計集計場所設定
               MOVE            8       TO    IDX2
             END-IF
           END-IF
      *
           .
       2001-IDX-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保データ集計処理
      *****************************************************************
       2001-DATA-SET2-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S138TMP-REC
      *
           MOVE    RECE10-NYUGAIKBN    TO    S138TMP-NYUGAIKBN
           MOVE    RECE10-RECEKA       TO    S138TMP-SRYKA
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     MOVE    RECE10-SRYYM       TO    S138TMP-SRYYM
           IF  RECE10-SRYYM < "200804"
             MOVE    "0"       TO    S138TMP-SRYYMKBN
           ELSE
             MOVE    "9"       TO    S138TMP-SRYYMKBN
           END-IF
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *    TMPファイル読み込み
           MOVE    2                   TO    FLG-TYPE
           PERFORM 800-SKTTMP-READ-SEC
      * 
      *    加算データ設定
           PERFORM  2001-KASAN-SEC
      *
      *    件数加算
           ADD    1                    TO    S138TMP-KENSU(IDX1)
                                             S138TMP-KENSU(IDX2)
                                             S138TMP-GKENSU
      *
      *    日数加算
           ADD    WRK-NISSU            TO    S138TMP-NISSU(IDX1)
                                             S138TMP-NISSU(IDX2)
                                             S138TMP-GNISSU
      *
      *    点数加算
           ADD    WRK-TENSU            TO    S138TMP-TENSU(IDX1)
                                             S138TMP-TENSU(IDX2)
                                             S138TMP-GTENSU
      *
      *    窓口請求額加算
           ADD    WRK-MADOGK           TO    S138TMP-MADOGK(IDX1)
                                             S138TMP-MADOGK(IDX2)
                                             S138TMP-GMADOGK
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計加算
           ADD    WRK-JIHIGK           TO    S138TMP-JIHIGK(IDX1)
                                             S138TMP-JIHIGK(IDX2)
                                             S138TMP-GJIHIGK
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額加算
           ADD    WRK-HKNGK            TO    S138TMP-HKNGK(IDX1)
                                             S138TMP-HKNGK(IDX2)
                                             S138TMP-GHKNGK
      *
      *    公費請求額
           ADD    WRK-KOHGK            TO    S138TMP-KOHGK(IDX1)
                                             S138TMP-KOHGK(IDX2)
                                             S138TMP-GKOHGK
      *
      *    医療収益
           ADD    WRK-SYUEKI           TO    S138TMP-SYUEKI(IDX1)
                                             S138TMP-SYUEKI(IDX2)
                                             S138TMP-GSYUEKI
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *
      *      日数加算
             ADD    WRK-SNISSU         TO    S138TMP-SNISSU(IDX1)
                                             S138TMP-SNISSU(IDX2)
                                             S138TMP-SGNISSU
      *      請求額加算
             ADD    WRK-SSEIKYU        TO    S138TMP-SSEIKYU(IDX1)
                                             S138TMP-SSEIKYU(IDX2)
                                             S138TMP-SGSEIKYU
      *      標準負担額加算
             ADD    WRK-SFTNGK         TO    S138TMP-SFTNGK(IDX1)
                                             S138TMP-SFTNGK(IDX2)
                                             S138TMP-SGFTNGK
      ***  20040820  @@@@-MOTOHASI  
      *
      *
      *    公費集計処理 
           IF    RECE10-KOHNUM(1)  NOT = SPACE
             PERFORM  2001-DATA-SET21-SEC
           END-IF
      *
      *    保険者毎の集計処理
           IF    RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)    TO    WRK-HKNJANUM
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)    TO    WRK-HKNJANUM
           END-IF
      *
           PERFORM  2001-DATA-SET22-SEC
      *
      *    書き出し
           IF  FLG-S138TMP   =  1
             ADD      1                    TO    CNT-DATA
             WRITE    S138TMP-REC
           ELSE
             REWRITE  S138TMP-REC
           END-IF
      *
      *    保険合計用TMPファイルに加算
      *     PERFORM  2001-DATA-SET3-SEC
      *
           .
       2001-DATA-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保公費集計処理
      *****************************************************************
       2001-DATA-SET21-SEC                SECTION.
      *
           PERFORM VARYING IDX5     FROM    1   BY  1
                   UNTIL   IDX5   >   4
                   OR  RECE10-KOHNUM (IDX5) =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
      ***  20040706  @@@@-MOTOHASI  変更
      ***                UNTIL    IDX4  >     50
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *                UNTIL    IDX4  >     100
                      UNTIL    IDX4  >     300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
                      OR       S138TMP-KOH1-KEY (IDX4) = SPACE
               IF  RECE10-KOHNUM (IDX5)  =  S138TMP-KOHNUM (IDX4) AND
                   RECE10-RJNHKNNUM      =  S138TMP-RJNKBN(IDX4)
                   MOVE  IDX4  TO  CNT-KOH
      ***  20040706  @@@@-MOTOHASI  変更
      ***             MOVE  50    TO  IDX4
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *             MOVE  100    TO  IDX4
                   MOVE  300    TO  IDX4
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
      ***  20040706  @@@@-MOTOHASI  変更
      ***       IF  IDX4  < 50
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *       IF  IDX4  < 100
             IF  IDX4  < 300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    老人併用区分設定
             MOVE  RECE10-RJNHKNNUM     TO  S138TMP-RJNKBN(CNT-KOH)
      *    公費名
             MOVE  RECE10-KOHNUM(IDX5)  TO  S138TMP-KOHNUM(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  S138TMP-KKENSU(CNT-KOH)
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX5 + 1)  TO
                               S138TMP-KNISSU(CNT-KOH)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX5 + 1)  TO
                               S138TMP-KTENSU(CNT-KOH)
      *
             ADD     1                          TO  WRK-KKENSU
             ADD     RECE10-JNISSU(IDX5 + 1)    TO  WRK-KNISSU
             ADD     RECE10-TOTALTEN(IDX5 + 1)  TO  WRK-KTENSU
      *
           END-PERFORM
           .
       2001-DATA-SET21-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保保険者毎の処理
      *****************************************************************
       2001-DATA-SET22-SEC                SECTION.
      *
           MOVE    1             TO    IDC
      ***  20040706  @@@@-MOTOHASI  変更
      ***     MOVE    50            TO    IDX-MAX
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *     MOVE    100            TO    IDX-MAX
           MOVE    300            TO    IDX-MAX
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
           PERFORM    UNTIL   IDC   >    IDX-MAX
             IF   S138TMP-HKNJANUM(IDC)   =  SPACE  OR WRK-HKNJANUM
               MOVE    WRK-HKNJANUM  TO   S138TMP-HKNJANUM(IDC)
               MOVE    ZERO      TO    IDX-MAX
             ELSE
               ADD     1         TO    IDC
             END-IF     
           END-PERFORM
      *
      *    保険者加算
      ***  20040706  @@@@-MOTOHASI  変更
      ***     IF    IDC    <=  50
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *     IF    IDC    <=  100
           IF    IDC    <=  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
      *    件数加算
             ADD    1                    TO    S138TMP-HKENSU(IDC)
      *
      *    日数加算
             ADD    WRK-NISSU            TO    S138TMP-HNISSU(IDC)
      *
      *    点数加算
             ADD    WRK-TENSU            TO    S138TMP-HTENSU(IDC)
      *
      *    窓口請求額加算
             ADD    WRK-MADOGK           TO    S138TMP-HMADOGK(IDC)
      *
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計加算
           ADD      WRK-JIHIGK           TO    S138TMP-HJIHIGK(IDC)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額加算
             ADD    WRK-HKNGK            TO    S138TMP-HHKNGK(IDC)
      *
      *    公費請求額
             ADD    WRK-KOHGK            TO    S138TMP-HKOHGK(IDC)
      *
      *    医療収益
             ADD    WRK-SYUEKI           TO    S138TMP-HSYUEKI(IDC)
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *
      *      日数加算
             ADD    WRK-SNISSU         TO    S138TMP-HSNISSU(IDC)
      *
      *      請求額加算
             ADD    WRK-SSEIKYU        TO    S138TMP-HSSEIKYU(IDC)
      *
      *      標準負担額加算
             ADD    WRK-SFTNGK         TO    S138TMP-HSFTNGK(IDC)
      *
      ***  20040820  @@@@-MOTOHASI
      *
           END-IF
      *
           .
       2001-DATA-SET22-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保データ集計処理(国保組合)
      *****************************************************************
       2001-DATA-SET2-K-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S138TMP-REC
      *
           MOVE    RECE10-NYUGAIKBN    TO    S138TMP-NYUGAIKBN
           MOVE    RECE10-RECEKA       TO    S138TMP-SRYKA
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     MOVE    RECE10-SRYYM       TO    S138TMP-SRYYM
           IF  RECE10-SRYYM < "200804"
             MOVE    "0"       TO    S138TMP-SRYYMKBN
           ELSE
             MOVE    "9"       TO    S138TMP-SRYYMKBN
           END-IF
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *    TMPファイル読み込み
           MOVE    2                   TO    FLG-TYPE
           PERFORM 800-SKTTMP-READ-SEC
      * 
      *    加算データ設定
           PERFORM  2001-KASAN-SEC
      *
      *    件数加算
           ADD    1                    TO    S138TMP-K-KENSU(IDX1)
                                             S138TMP-K-KENSU(IDX2)
                                             S138TMP-GKENSU
      *
      *    日数加算
           ADD    WRK-NISSU            TO    S138TMP-K-NISSU(IDX1)
                                             S138TMP-K-NISSU(IDX2)
                                             S138TMP-GNISSU
      *
      *    点数加算
           ADD    WRK-TENSU            TO    S138TMP-K-TENSU(IDX1)
                                             S138TMP-K-TENSU(IDX2)
                                             S138TMP-GTENSU
      *
      *    窓口請求額加算
           ADD    WRK-MADOGK           TO    S138TMP-K-MADOGK(IDX1)
                                             S138TMP-K-MADOGK(IDX2)
                                             S138TMP-GMADOGK
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計加算
           ADD    WRK-JIHIGK           TO    S138TMP-K-JIHIGK(IDX1)
                                             S138TMP-K-JIHIGK(IDX2)
                                             S138TMP-GJIHIGK
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額加算
           ADD    WRK-HKNGK            TO    S138TMP-K-HKNGK(IDX1)
                                             S138TMP-K-HKNGK(IDX2)
                                             S138TMP-GHKNGK
      *
      *    公費請求額
           ADD    WRK-KOHGK            TO    S138TMP-K-KOHGK(IDX1)
                                             S138TMP-K-KOHGK(IDX2)
                                             S138TMP-GKOHGK
      *
      *    医療収益
           ADD    WRK-SYUEKI           TO    S138TMP-K-SYUEKI(IDX1)
                                             S138TMP-K-SYUEKI(IDX2)
                                             S138TMP-GSYUEKI
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *
      *      日数加算
             ADD    WRK-SNISSU         TO    S138TMP-K-SNISSU(IDX1)
                                             S138TMP-K-SNISSU(IDX2)
                                             S138TMP-SGNISSU
      *      請求額加算
             ADD    WRK-SSEIKYU        TO    S138TMP-K-SSEIKYU(IDX1)
                                             S138TMP-K-SSEIKYU(IDX2)
                                             S138TMP-SGSEIKYU
      *      標準負担額加算
             ADD    WRK-SFTNGK         TO    S138TMP-K-SFTNGK(IDX1)
                                             S138TMP-K-SFTNGK(IDX2)
                                             S138TMP-SGFTNGK
      ***  20040820  @@@@-MOTOHASI  
      *
      *
      *    公費集計処理 
           IF    RECE10-KOHNUM(1)  NOT = SPACE
             PERFORM  2001-DATA-SET21-K-SEC
           END-IF
      *
      *    保険者毎の集計処理
           IF    RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)    TO    WRK-HKNJANUM
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)    TO    WRK-HKNJANUM
           END-IF
      *
           PERFORM  2001-DATA-SET22-K-SEC
      *
      *    書き出し
           IF  FLG-S138TMP   =  1
             ADD      1                  TO    CNT-DATA
             WRITE    S138TMP-REC
           ELSE
             REWRITE  S138TMP-REC
           END-IF
      *
      *    保険合計用TMPファイルに加算
      *     PERFORM  2001-DATA-SET3-SEC
      *
           .
       2001-DATA-SET2-K-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保公費集計処理(国保組合)
      *****************************************************************
       2001-DATA-SET21-K-SEC                SECTION.
      *
           PERFORM VARYING IDX5     FROM    1   BY  1
                   UNTIL   IDX5     >       4
                   OR  RECE10-KOHNUM (IDX5) =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
      ***  20040706  @@@@-MOTOHASI  変更
      ***                UNTIL    IDX4  >     50
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *                UNTIL    IDX4  >     100
                      UNTIL    IDX4  >     300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
                      OR       S138TMP-KOH2-KEY (IDX4) = SPACE
               IF  RECE10-KOHNUM(IDX5)  =  S138TMP-K-KOHNUM(IDX4)
                   AND  RECE10-RJNHKNNUM  =  S138TMP-K-RJNKBN(IDX4)
                 MOVE    IDX4      TO   CNT-KOH
      ***  20040706  @@@@-MOTOHASI  変更
      ***             MOVE  50    TO  IDX4
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *             MOVE  100    TO  IDX4
                 MOVE    300       TO   IDX4
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
      ***  20040706  @@@@-MOTOHASI  変更
      ***       IF  IDX4  < 50
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *       IF  IDX4  < 100
             IF  IDX4  < 300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    老人併用区分設定
             MOVE  RECE10-RJNHKNNUM     TO  S138TMP-K-RJNKBN(CNT-KOH)
      *    公費名
             MOVE  RECE10-KOHNUM(IDX5)  TO  S138TMP-K-KOHNUM(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  S138TMP-K-KKENSU(CNT-KOH)
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX5 + 1)  TO
                               S138TMP-K-KNISSU(CNT-KOH)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX5 + 1)  TO
                               S138TMP-K-KTENSU(CNT-KOH)
      *
             ADD     1                          TO  WRK-KKENSU
             ADD     RECE10-JNISSU(IDX5 + 1)    TO  WRK-KNISSU
             ADD     RECE10-TOTALTEN(IDX5 + 1)  TO  WRK-KTENSU
      *
           END-PERFORM
           .
       2001-DATA-SET21-K-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保保険者毎の処理(国保組合)
      *****************************************************************
       2001-DATA-SET22-K-SEC                SECTION.
      *
           MOVE    1             TO    IDC
      ***  20040706  @@@@-MOTOHASI  変更
      ***     MOVE    50            TO    IDX-MAX
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *     MOVE    100            TO    IDX-MAX
           MOVE    300            TO    IDX-MAX
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
           PERFORM    UNTIL   IDC   >    IDX-MAX
             IF   S138TMP-K-HKNJANUM(IDC)   =  SPACE  OR WRK-HKNJANUM
               MOVE    WRK-HKNJANUM  TO   S138TMP-K-HKNJANUM(IDC)
               MOVE    ZERO      TO    IDX-MAX
             ELSE
               ADD     1         TO    IDC
             END-IF   
           END-PERFORM
      *
      *    保険者加算
      ***  20040706  @@@@-MOTOHASI  変更
      ***     IF    IDC    <=  50
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *     IF    IDC    <=  100
           IF    IDC    <=  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
      *    件数加算
             ADD    1                    TO    S138TMP-K-HKENSU(IDC)
      *
      *    日数加算
             ADD    WRK-NISSU            TO    S138TMP-K-HNISSU(IDC)
      *
      *    点数加算
             ADD    WRK-TENSU            TO    S138TMP-K-HTENSU(IDC)
      *
      *    窓口請求額加算
             ADD    WRK-MADOGK           TO    S138TMP-K-HMADOGK(IDC)
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計加算
             ADD    WRK-JIHIGK           TO    S138TMP-K-HJIHIGK(IDC)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額加算
             ADD    WRK-HKNGK            TO    S138TMP-K-HHKNGK(IDC)
      *
      *    公費請求額
             ADD    WRK-KOHGK            TO    S138TMP-K-HKOHGK(IDC)
      *
      *    医療収益
             ADD    WRK-SYUEKI           TO    S138TMP-K-HSYUEKI(IDC)
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *
      *    日数加算
             ADD    WRK-SNISSU         TO    S138TMP-K-HSNISSU(IDC)
      *
      *    請求額加算
             ADD    WRK-SSEIKYU        TO    S138TMP-K-HSSEIKYU(IDC)
      *
      *    標準負担額加算
             ADD    WRK-SFTNGK         TO    S138TMP-K-HSFTNGK(IDC)
      *
      ***  20040820  @@@@-MOTOHASI
      *
           END-IF
      *
      *
           .
       2001-DATA-SET22-K-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算データ設定処理
      *****************************************************************
       2001-KASAN-SEC                SECTION.
      *
      *
           INITIALIZE    WRK-KASAN-TBL
      *
           MOVE    RECE10-JNISSU(1)    TO    WRK-NISSU
           MOVE    RECE10-TOTALTEN(1)  TO    WRK-TENSU
      *
      *    限度額が有る場合は(点数×１０) − 患者負担 で算出する
           IF    RECE10-FTNMONEY(1)  =  ZERO
      ***  20050419  @@@@-MOTOHASI  
                   AND  RECE10-KYURATE  NOT = ZERO
      ***  20050419  @@@@-MOTOHASI  
             COMPUTE  WRK-HKNGK  =  RECE10-TOTALTEN(1)
                                  * RECE10-KYURATE
           ELSE
             COMPUTE  WRK-HKNGK  =  (RECE10-TOTALTEN(1) * 10)
                                 -  RECE10-FTNMONEY(1)
           END-IF
      *
      *    収納情報から窓口請求額、公費請求額を集計
           MOVE    ZERO                    TO    WRK-MADOSEIKYUGK-G
                                                 WRK-KOHSEIKYUGK-G
      *--- (2008/12/12) line add start monma ---------------------------
                                                 WRK-JIHIGK-G
      *--- (2008/12/12) line add end   monma ---------------------------
      *
           PERFORM  920-SYUNOU-SET-SEC
      *--- (2008/12/12) line add&del start monma -----------------------
      *    その他自費を別集計
           MOVE    WRK-JIHIGK-G             TO    WRK-JIHIGK
           COMPUTE WRK-MADOGK  =  WRK-MADOSEIKYUGK-G  -  WRK-JIHIGK-G
      *     MOVE    WRK-MADOSEIKYUGK-G       TO    WRK-MADOGK
      *--- (2008/12/12) line add&del end   monma -----------------------
      *
           MOVE    WRK-KOHSEIKYUGK-G        TO    WRK-KOHGK
      *
      *    公費単独の場合の編集
           IF    RECE10-HKNNUM  =  SPACE
             MOVE    RECE10-JNISSU(2)    TO    WRK-NISSU
             MOVE    RECE10-TOTALTEN(2)  TO    WRK-TENSU
             COMPUTE  WRK-HKNGK  = (RECE10-TOTALTEN(2) * 10)
                                 + RECE10-SHOKUJIRYOYOHI(2)
             ADD     WRK-HKNGK           TO    WRK-KOHGK
             MOVE    ZERO                TO    WRK-HKNGK
           END-IF
      *
      *--- (2008/12/12) line del start monma ---------------------------
      *    医療収益
      *     COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
      *                          +  WRK-KOHGK
      *--- (2008/12/12) line del end   monma ---------------------------
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *    日数
             MOVE  RECE10-SHOKUJINISSU(1)     TO    WRK-SNISSU
      *    請求額
             MOVE  RECE10-SHOKUJIRYOYOHI(1)   TO    WRK-SSEIKYU
      *    標準負担額
             MOVE  RECE10-SHOKUJIFTN(1)       TO    WRK-SFTNGK
      ***  20040820  @@@@-MOTOHASI  
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    医療収益 食事収益加算
           COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                +  WRK-KOHGK  +  WRK-JIHIGK
                                +  (WRK-SSEIKYU - WRK-SFTNGK)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
           .
       2001-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *    国保データ印刷処理
           PERFORM 2002-PRINT-TYPE2-SEC
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保データ印刷処理
      *****************************************************************
       2002-PRINT-TYPE2-SEC                SECTION.
      *
           OPEN     INPUT        S138TMP-FILE
           MOVE     ZERO    TO   FLG-PRINT
           INITIALIZE            S138TMP-REC
      *
           MOVE    2        TO   FLG-TYPE
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT   =    1
      *
      *
      ***  20040820  @@@@-MOTOHASI  追加
             IF    WRK-PARA-NYUGAIKBN   =  "1"  OR  SPACE
               IF    S138TMP-NYUGAIKBN   =   "1"
      ***  20040820  @@@@-MOTOHASI
      *          入院フォーム
      *          １枚目
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
      *       PERFORM  UNTIL  FLG-PASS   =   ZERO
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S138TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
                          AND    S138TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
      *
                   PERFORM  3104-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3205-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  350-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      *
      *          ２枚目（国保分）
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
      *       PERFORM  UNTIL  FLG-PASS   =   ZERO
                 PERFORM  UNTIL  FLG-PASS   =   1
                        AND (   S138TMP-HKNJANUM(WRK-CNT3)  =  '000000'
                           OR    S138TMP-HKNJANUM(WRK-CNT3)  =  SPACE )
                   MOVE   "（市町村）"  TO      WRK-CITYNAME
                   PERFORM  3105-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3206-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  360-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      *
      *          ３枚目（国保組合分）
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
      *       PERFORM  UNTIL  FLG-PASS   =   ZERO
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S138TMP-K-HKNJANUM(WRK-CNT4)  =  SPACE
                   MOVE   "（国保組合）"  TO      WRK-CITYNAME
                   PERFORM  3105-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3207-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  360-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
               END-IF
             END-IF
      *
      ***  20040820  @@@@-MOTOHASI  追加
             IF    WRK-PARA-NYUGAIKBN     =  "2"  OR  SPACE
               IF    S138TMP-NYUGAIKBN      =    "2"
      ***  20040820  @@@@-MOTOHASI  
      *          外来フォーム
      *          １枚目
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
      *       PERFORM  UNTIL  FLG-PASS   =   ZERO
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S138TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
                          AND    S138TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
      *
                   PERFORM  3102-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3202-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  330-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      *
      *          ２枚目（国保分）
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
      *       PERFORM  UNTIL  FLG-PASS   =   ZERO
                 PERFORM  UNTIL  FLG-PASS   =   1
                        AND (   S138TMP-HKNJANUM(WRK-CNT3)  =  '000000'
                           OR    S138TMP-HKNJANUM(WRK-CNT3)  =  SPACE )
                   MOVE   "（市町村）"  TO      WRK-CITYNAME
                   PERFORM  3103-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3203-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  340-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      *
      *          ３枚目（国保組合分）
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
      *       PERFORM  UNTIL  FLG-PASS   =   ZERO
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S138TMP-K-HKNJANUM(WRK-CNT4)  =  SPACE
                   MOVE   "（国保組合）"  TO      WRK-CITYNAME
                   PERFORM  3103-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3204-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  340-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
               END-IF
             END-IF
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    S138TMP-FILE
      *
           .
       2002-PRINT-TYPE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<ヘッダー部>処理
      *****************************************************************
       3102-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S138G1
      *
      *    ページ数
      * 20040322 編集 Y.Takahashi
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S138G1-PAGE
      *      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF  S138TMP-SRYKA       =     SUM-SRYKA(IDX4)
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO    S138G1-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S138TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S138G1-NYUGAI
           ELSE
             MOVE     "外来"        TO   S138G1-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S138G1-SRYYM
           MOVE    WRK-SEIYMD       TO   S138G1-SYSYMD
      *
           .
       3102-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<ヘッダー部>処理
      *****************************************************************
       3103-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S138G2
      *
      *    ページ数
      * 20040322 編集 Y.Takahashi
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S138G2-PAGE
      *      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF  S138TMP-SRYKA       =     SUM-SRYKA(IDX4)
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO    S138G2-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S138TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S138G2-NYUGAI
           ELSE
             MOVE     "外来"        TO   S138G2-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S138G2-SRYYM
           MOVE    WRK-SEIYMD       TO   S138G2-SYSYMD
      *
           MOVE    WRK-CITYNAME     TO   S138G2-CITYKBN
      *
           .
       3103-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<ヘッダー部>処理
      *****************************************************************
       3104-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S138N1
      *
      *    ページ数
      * 20040322 編集 Y.Takahashi
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S138N1-PAGE
      *      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF  S138TMP-SRYKA       =     SUM-SRYKA(IDX4)
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO    S138N1-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S138TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S138N1-NYUGAI
           ELSE
             MOVE     "外来"        TO   S138N1-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S138N1-SRYYM
           MOVE    WRK-SEIYMD       TO   S138N1-SYSYMD
      *
           .
       3104-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<ヘッダー部>処理
      *****************************************************************
       3105-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S138N2
      *
      *    ページ数
      * 20040322 編集 Y.Takahashi
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S138N2-PAGE
      *      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF  S138TMP-SRYKA       =     SUM-SRYKA(IDX4)
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO    S138N2-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S138TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S138N2-NYUGAI
           ELSE
             MOVE     "外来"        TO   S138N2-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S138N2-SRYYM
           MOVE    WRK-SEIYMD       TO   S138N2-SYSYMD
      *
           MOVE    WRK-CITYNAME     TO   S138N2-CITYKBN
      *
           .
       3105-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(外来：１枚目)
      *****************************************************************
       3202-BODY-HEN-SEC               SECTION.
      *
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     14
                      OR       FLG-PASS  = 1
      *    国保分
      *    件数
             MOVE     S138TMP-KENSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G1-KENSU(IDX1)
      *
      *    日数
             MOVE     S138TMP-NISSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G1-NISSU(IDX1)
      *
      *    点数
             MOVE     S138TMP-TENSU(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-TENSU(IDX1)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *       MOVE     S138TMP-MADOGK(IDX1)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO    S138G1-MADOGK(IDX1)
             MOVE     S138TMP-MADOGK(IDX1)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S138G1-MADOGK(IDX1)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
             MOVE     S138TMP-JIHIGK(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-JIHIGK(IDX1)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S138TMP-HKNGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-HKNGK(IDX1)
      *
      *    公費請求額
             MOVE     S138TMP-KOHGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-KOHGK(IDX1)
      *
      *    医療収益
             MOVE     S138TMP-SYUEKI(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-SYUEKI(IDX1)
      *
      *    国保組合分
      *    件数
             IF    IDX1    <=    11
               MOVE     S138TMP-K-KENSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO    S138G1-K-KENSU(IDX1)
      *
      *    日数
               MOVE     S138TMP-K-NISSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO    S138G1-K-NISSU(IDX1)
      *
      *    点数
               MOVE     S138TMP-K-TENSU(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138G1-K-TENSU(IDX1)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE     S138TMP-K-MADOGK(IDX1)   TO    WRK-Z92
      *         MOVE     WRK-Z92              TO    S138G1-K-MADOGK(IDX1)
               MOVE     S138TMP-K-MADOGK(IDX1)   TO    WRK-Z93
               MOVE     WRK-Z93              TO    S138G1-K-MADOGK(IDX1)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
               MOVE     S138TMP-K-JIHIGK(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92              TO    S138G1-K-JIHIGK(IDX1)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE     S138TMP-K-HKNGK(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138G1-K-HKNGK(IDX1)
      *
      *    公費請求額
               MOVE     S138TMP-K-KOHGK(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138G1-K-KOHGK(IDX1)
      *
      *    医療収益
               MOVE     S138TMP-K-SYUEKI(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92              TO    S138G1-K-SYUEKI(IDX1)
             END-IF
           END-PERFORM
      *
      *    合計
           IF    FLG-PASS  =  ZERO
      *
      *    件数
             MOVE     S138TMP-GKENSU          TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-GKENSU
      *
      *    日数
             MOVE     S138TMP-GNISSU          TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-GNISSU
      *
      *    点数
             MOVE     S138TMP-GTENSU          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GTENSU
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *       MOVE     S138TMP-GMADOGK         TO    WRK-Z92
      *       MOVE     WRK-Z92                TO    S138G1-GMADOGK
             MOVE     S138TMP-GMADOGK         TO    WRK-Z93
             MOVE     WRK-Z93                TO    S138G1-GMADOGK
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
             MOVE     S138TMP-GJIHIGK         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GJIHIGK
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S138TMP-GHKNGK          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GHKNGK
      *
      *    公費請求額
             MOVE     S138TMP-GKOHGK          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GKOHGK
      *
      *    医療収益
             MOVE     S138TMP-GSYUEKI         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GSYUEKI
           END-IF
      *
      *    国保・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     20
                    OR       S138TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *              OR       WRK-CNT1  >  100
                    OR       WRK-CNT1  >  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S138TMP-KOH1-KEY(WRK-CNT1)  TO WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G  TO
                                                   S138G1-KOHNAME(IDX3)
      *    件数
             MOVE     S138TMP-KKENSU(WRK-CNT1)   TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-KKENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-KNISSU(WRK-CNT1)      TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-KNISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-KTENSU(WRK-CNT1)      TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT1
           END-PERFORM     
      *
      *
      *    組合・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     20
                    OR       S138TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *              OR       WRK-CNT2  >  100
                    OR       WRK-CNT2  >  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S138TMP-KOH2-KEY(WRK-CNT2)  TO WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G  TO
                                                  S138G1-K-KOHNAME(IDX3)
      *    件数
             MOVE     S138TMP-K-KKENSU(WRK-CNT2)   TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-K-KKENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-K-KNISSU(WRK-CNT2)      TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-K-KNISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-K-KTENSU(WRK-CNT2)      TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-K-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT2
           END-PERFORM     
      *
      *
           .
       3202-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(外来：２枚目（国保分）)
      *****************************************************************
       3203-BODY-HEN-SEC               SECTION.
      *
      *
      *    国保・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR       S138TMP-HKNJANUM(WRK-CNT3)  =  SPACE
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *              OR       WRK-CNT3  >  100
                    OR       WRK-CNT3  >  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S138TMP-HKNJANUM(WRK-CNT3)  TO WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME  TO
                                                  S138G2-HKNJANAME(IDX3)
      *    件数
             MOVE     S138TMP-HKENSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G2-KENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-HNISSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G2-NISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-HTENSU(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-TENSU(IDX3)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *       MOVE     S138TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO    S138G2-MADOGK(IDX3)
             MOVE     S138TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S138G2-MADOGK(IDX3)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
             MOVE     S138TMP-HJIHIGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-JIHIGK(IDX3)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S138TMP-HHKNGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S138TMP-HKOHGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S138TMP-HSYUEKI(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-SYUEKI(IDX3)
             ADD      1                      TO    WRK-CNT3
           END-PERFORM     
      *
      *
           .
       3203-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(外来：３枚目（国保組合分）)
      *****************************************************************
       3204-BODY-HEN-SEC               SECTION.
      *
      *
      *    組合・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR       S138TMP-K-HKNJANUM(WRK-CNT4)  =  SPACE
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *              OR       WRK-CNT4  >  100
                    OR       WRK-CNT4  >  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S138TMP-K-HKNJANUM(WRK-CNT4)  TO WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME  TO
                                               S138G2-HKNJANAME(IDX3)
      *    件数
             MOVE     S138TMP-K-HKENSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G2-KENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-K-HNISSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G2-NISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-K-HTENSU(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-TENSU(IDX3)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *       MOVE     S138TMP-K-HMADOGK(WRK-CNT4)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO   S138G2-MADOGK(IDX3)
             MOVE     S138TMP-K-HMADOGK(WRK-CNT4)   TO    WRK-Z93
             MOVE     WRK-Z93               TO   S138G2-MADOGK(IDX3)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
             MOVE     S138TMP-K-HJIHIGK(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO   S138G2-JIHIGK(IDX3)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S138TMP-K-HHKNGK(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S138TMP-K-HKOHGK(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S138TMP-K-HSYUEKI(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-SYUEKI(IDX3)
             ADD      1                      TO    WRK-CNT4
           END-PERFORM     
      *
           .
       3204-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(入院：１枚目)
      *****************************************************************
       3205-BODY-HEN-SEC               SECTION.
      *
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     14
                      OR       FLG-PASS  = 1
      *    国保分
      *    件数
             MOVE     S138TMP-KENSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N1-KENSU(IDX1)
      *
      *    日数
             MOVE     S138TMP-NISSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N1-NISSU(IDX1)
      *
      *    点数
             MOVE     S138TMP-TENSU(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-TENSU(IDX1)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *       MOVE     S138TMP-MADOGK(IDX1)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO    S138N1-MADOGK(IDX1)
             MOVE     S138TMP-MADOGK(IDX1)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S138N1-MADOGK(IDX1)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
             MOVE     S138TMP-JIHIGK(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-JIHIGK(IDX1)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S138TMP-HKNGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-HKNGK(IDX1)
      *
      *    公費請求額
             MOVE     S138TMP-KOHGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-KOHGK(IDX1)
      *
      *    医療収益
             MOVE     S138TMP-SYUEKI(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SYUEKI(IDX1)
      ***  食事療養
      *    日数
             MOVE     S138TMP-SNISSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N1-SNISSU(IDX1)
      *
      *    請求額
             MOVE     S138TMP-SSEIKYU(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SSEIKYU(IDX1)
      *
      *    標準負担額
             MOVE     S138TMP-SFTNGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SFTNGK(IDX1)
           END-PERFORM
      *
      *
      *    国保・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     20
                    OR       S138TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
      ***  20040706  @@@@-MOTOHASI  変更
      ***              OR       WRK-CNT1  >  50
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *              OR       WRK-CNT1  >  100
                    OR       WRK-CNT1  >  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      ***  20040706  ここまで
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S138TMP-KOH1-KEY(WRK-CNT1)  TO WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G  TO
                                                   S138N1-KOHNAME(IDX3)
      *    件数
             MOVE     S138TMP-KKENSU(WRK-CNT1)   TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-KKENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-KNISSU(WRK-CNT1)      TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-KNISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-KTENSU(WRK-CNT1)      TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT1
           END-PERFORM     
      *
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     14
                      OR       FLG-PASS  = 1
      *
      *    国保組合分
      *    件数
             IF    IDX1    <=    11
               MOVE     S138TMP-K-KENSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO    S138N1-K-KENSU(IDX1)
      *
      *    日数
               MOVE     S138TMP-K-NISSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO    S138N1-K-NISSU(IDX1)
      *
      *    点数
               MOVE     S138TMP-K-TENSU(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138N1-K-TENSU(IDX1)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *         MOVE     S138TMP-K-MADOGK(IDX1)   TO    WRK-Z92
      *         MOVE     WRK-Z92               TO   S138N1-K-MADOGK(IDX1)
               MOVE     S138TMP-K-MADOGK(IDX1)   TO    WRK-Z93
               MOVE     WRK-Z93               TO   S138N1-K-MADOGK(IDX1)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計
               MOVE     S138TMP-K-JIHIGK(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO   S138N1-K-JIHIGK(IDX1)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
               MOVE     S138TMP-K-HKNGK(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138N1-K-HKNGK(IDX1)
      *
      *    公費請求額
               MOVE     S138TMP-K-KOHGK(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138N1-K-KOHGK(IDX1)
      *
      *    医療収益
               MOVE     S138TMP-K-SYUEKI(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO   S138N1-K-SYUEKI(IDX1)
      ***  食事療養
      *    日数
               MOVE     S138TMP-K-SNISSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO   S138N1-K-SNISSU(IDX1)
      *
      *    請求額
               MOVE     S138TMP-K-SSEIKYU(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO  S138N1-K-SSEIKYU(IDX1)
      *
      *    標準負担額
               MOVE     S138TMP-K-SFTNGK(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO   S138N1-K-SFTNGK(IDX1)
             END-IF
           END-PERFORM
      *
      *    組合・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     20
                    OR       S138TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *              OR       WRK-CNT2  >  100
                    OR       WRK-CNT2  >  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S138TMP-KOH2-KEY(WRK-CNT2)  TO WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G  TO
                                                  S138N1-K-KOHNAME(IDX3)
      *    件数
             MOVE     S138TMP-K-KKENSU(WRK-CNT2)   TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-K-KKENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-K-KNISSU(WRK-CNT2)      TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-K-KNISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-K-KTENSU(WRK-CNT2)      TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-K-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT2
           END-PERFORM     
      *
      *    合計
           IF    FLG-PASS  =  ZERO
      *
      *    件数
             MOVE     S138TMP-GKENSU          TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-GKENSU
      *
      *    日数
             MOVE     S138TMP-GNISSU          TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-GNISSU
      *
      *    点数
             MOVE     S138TMP-GTENSU          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GTENSU
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *       MOVE     S138TMP-GMADOGK         TO    WRK-Z92
      *       MOVE     WRK-Z92                TO    S138N1-GMADOGK
             MOVE     S138TMP-GMADOGK         TO    WRK-Z93
             MOVE     WRK-Z93                TO    S138N1-GMADOGK
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
             MOVE     S138TMP-GJIHIGK         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GJIHIGK
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S138TMP-GHKNGK          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GHKNGK
      *
      *    公費請求額
             MOVE     S138TMP-GKOHGK          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GKOHGK
      *
      *    医療収益
             MOVE     S138TMP-GSYUEKI         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GSYUEKI
      ***  食事療養
      *    日数
             MOVE     S138TMP-SGNISSU    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N1-SGNISSU
      *
      *    請求額
             MOVE     S138TMP-SGSEIKYU   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SGSEIKYU
      *
      *    標準負担額
             MOVE     S138TMP-SGFTNGK    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SGFTNGK
           END-IF
      *
      *
           .
       3205-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(入院：２枚目（国保分）)
      *****************************************************************
       3206-BODY-HEN-SEC               SECTION.
      *
      *
      *    国保・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR       S138TMP-HKNJANUM(WRK-CNT3)  =  SPACE
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *              OR       WRK-CNT3  >  100
                    OR       WRK-CNT3  >  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S138TMP-HKNJANUM(WRK-CNT3)  TO WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME  TO
                                                  S138N2-HKNJANAME(IDX3)
      *    件数
             MOVE     S138TMP-HKENSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-KENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-HNISSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-NISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-HTENSU(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-TENSU(IDX3)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *       MOVE     S138TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO    S138N2-MADOGK(IDX3)
             MOVE     S138TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S138N2-MADOGK(IDX3)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
             MOVE     S138TMP-HJIHIGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-JIHIGK(IDX3)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S138TMP-HHKNGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S138TMP-HKOHGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S138TMP-HSYUEKI(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SYUEKI(IDX3)
      ***  食事療養
      *    日数
             MOVE     S138TMP-HSNISSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-SNISSU(IDX3)
      *
      *    請求額
             MOVE     S138TMP-HSSEIKYU(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SSEIKYU(IDX3)
      *
      *    標準負担額
             MOVE     S138TMP-HSFTNGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SFTNGK(IDX3)
             ADD      1                      TO    WRK-CNT3
           END-PERFORM     
      *
      *
           .
       3206-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(入院：３枚目（国保組合分）)
      *****************************************************************
       3207-BODY-HEN-SEC               SECTION.
      *
      *
      *    組合・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR       S138TMP-K-HKNJANUM(WRK-CNT4)  =  SPACE
      * 20080625 保険者、公費情報領域100 → 300に変更START tanaka
      *              OR       WRK-CNT4  >  100
                    OR       WRK-CNT4  >  300
      * 20080625 保険者、公費情報領域100 → 300に変更END
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S138TMP-K-HKNJANUM(WRK-CNT4)  TO WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME  TO
                                               S138N2-HKNJANAME(IDX3)
      *    件数
             MOVE     S138TMP-K-HKENSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-KENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-K-HNISSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-NISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-K-HTENSU(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-TENSU(IDX3)
      *
      *--- (2008/12/26) line change start monma ------------------------
      *    窓口請求額
      *       MOVE     S138TMP-K-HMADOGK(WRK-CNT4)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO   S138N2-MADOGK(IDX3)
             MOVE     S138TMP-K-HMADOGK(WRK-CNT4)   TO    WRK-Z93
             MOVE     WRK-Z93               TO   S138N2-MADOGK(IDX3)
      *--- (2008/12/26) line change end   monma ------------------------
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費
             MOVE     S138TMP-K-HJIHIGK(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO   S138N2-JIHIGK(IDX3)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S138TMP-K-HHKNGK(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S138TMP-K-HKOHGK(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S138TMP-K-HSYUEKI(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SYUEKI(IDX3)
      ***  食事療養
      *    日数
             MOVE     S138TMP-K-HSNISSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-SNISSU(IDX3)
      *
      *    請求額
             MOVE     S138TMP-K-HSSEIKYU(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO   S138N2-SSEIKYU(IDX3)
      *
      *    標準負担額
             MOVE     S138TMP-K-HSFTNGK(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SFTNGK(IDX3)
             ADD      1                      TO    WRK-CNT4
           END-PERFORM     
      *
           .
       3207-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
      *
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE   "A00000S138G1"         TO  SPRT-PRTID
      * 20080423 H20年4月改正対応START tanaka
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     MOVE    "A00000S138G1.red"        TO  SPRT-PRTID
      *     IF S100LNK-YM < "200804"
      *         MOVE    "A00000S138G1O.red"        TO  SPRT-PRTID
      *     ELSE
           IF  S138TMP-SRYYMKBN  =  "0"
      * 20080512 返戻が正しく印字されない不具合を修正END
               MOVE    "A00000S138G1O.red"        TO  SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
           ELSE
               MOVE    "A00000S138G1N.red"        TO  SPRT-PRTID
           END-IF
      *     END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      * 20080423 H20年4月改正対応END
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE  TO  SPRT-TITLE
           MOVE    S138G1                TO  SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  SPRT-RETURN         =   ZERO
               CONTINUE               ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                     TO  WRK-RECEERR
	           PERFORM 500-ERR-HENSYU-SEC                           
	       END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.

      *
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE   "A00000S138G2"         TO  SPRT-PRTID
           MOVE   "A00000S138G2.red"         TO  SPRT-PRTID
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE  TO  SPRT-TITLE
           MOVE    S138G2                TO  SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  SPRT-RETURN         =   ZERO
               CONTINUE               ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                     TO  WRK-RECEERR
	           PERFORM 500-ERR-HENSYU-SEC                           
	       END-IF
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       350-PRINT-OUT-SEC         SECTION.
      *
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE   "A00000S138N1"         TO  SPRT-PRTID
      * 20080423 H20年4月改正対応START tanaka
      *     MOVE    "A00000S138N1.red"        TO  SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     IF S100LNK-YM < "200804"
      *         MOVE    "A00000S138N1O.red"        TO  SPRT-PRTID
      *     ELSE
           IF  S138TMP-SRYYMKBN  =  "0"
      * 20080512 返戻が正しく印字されない不具合を修正END
               MOVE    "A00000S138N1O.red"        TO  SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
           ELSE
               MOVE    "A00000S138N1N.red"        TO  SPRT-PRTID
           END-IF
      *     END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      * 20080423 H20年4月改正対応END
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE  TO  SPRT-TITLE
           MOVE    S138N1                TO  SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  SPRT-RETURN         =   ZERO
               CONTINUE               ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                     TO  WRK-RECEERR
	           PERFORM 500-ERR-HENSYU-SEC                           
	       END-IF
           .
       350-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       360-PRINT-OUT-SEC         SECTION.
      *
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE   "A00000S138N2"         TO  SPRT-PRTID
           MOVE   "A00000S138N2.red"         TO  SPRT-PRTID
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE  TO  SPRT-TITLE
           MOVE    S138N2                TO  SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  SPRT-RETURN         =   ZERO
               CONTINUE               ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                     TO  WRK-RECEERR
	           PERFORM 500-ERR-HENSYU-SEC                           
	       END-IF
           .
       360-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    公費名編集処理
      *****************************************************************
       333-KOHNAME-HEN-SEC               SECTION.
      *
           MOVE    SPACE       TO    WRK-KOHNAME-G
                                     WRK-KOHNAME-G2
      *
           IF    WRK-KOHTMP-RJNKBN  NOT = SPACE
             MOVE  "２７＋"    TO    WRK-KOHNAME-G
           END-IF
      *
           MOVE    WRK-KOHTMP-KOHNUM    TO  WRK-KOHNUM
           PERFORM  950-HKNNUM-SET-SEC
           IF    WRK-KOHNUM    =    "051"
             MOVE    "特定疾患"    TO    HKN-TANSEIDONAME
           END-IF
           STRING  WRK-KOHNAME-G      DELIMITED BY SPACE
                   HKN-HBTNUM         DELIMITED BY SPACE
                   "("                DELIMITED BY SIZE
                   HKN-TANSEIDONAME   DELIMITED BY SPACE
                   ")"                DELIMITED BY SIZE
                   INTO               WRK-KOHNAME-G2
           END-STRING
      *
           MOVE    WRK-KOHNAME-G2        TO    WRK-KOHNAME-G
      *
      ***  20040917 MOTOHASI
           MOVE   WRK-KOHNAME-G          TO    WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE   WRK-OUT-INPUT          TO    WRK-KOHNAME-G
      ***  20040917 MOTOHASI
      *    
      *     
           .
       333-KOHNAME-HEN-EXT.
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
           MOVE    "key"               TO  MCP-PATHNAME
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
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
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
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
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
           MOVE    SYS-1005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
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
          IF      MCP-RC           =   ZERO
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
           MOVE    "DBFETCH"                  TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
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
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1005-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
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
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
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
       820-SYSKANRI-DBCLOSE-EXT.
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
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
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
      *        ジョブ終了処理
               MOVE    "JBE"             TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
               MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
               MOVE    S100LNK-JOBID     TO  JOB-JOBID
               MOVE    S100LNK-SHELLID   TO  JOB-SHELLID
               MOVE    WRK-RECEERR       TO  JOB-YOBI
               MOVE    "9999"            TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           END-IF
      *
           MOVE    1                   TO  FLG-END
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
             READ    S138TMP-FILE
                 INVALID    KEY
                     MOVE    1           TO  FLG-S138TMP
                 NOT INVALID  KEY
                     MOVE    ZERO        TO  FLG-S138TMP
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
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
           IF      MCP-RC              =   ZERO
                MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *          MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
      *          CALL    "ORCMCPSUB"          USING
                MOVE    "tbl_syskanri"      TO  MCP-TABLE
                MOVE    "key10"             TO  MCP-PATHNAME
                CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                             MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                       ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                             MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                             SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
                IF      MCP-RC              =   ZERO
                    MOVE    ZERO            TO  FLG-SYSKANRI
                ELSE
                    MOVE    1               TO  FLG-SYSKANRI
                END-IF
           ELSE
                MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 Takashima(2006/08/09)
      *      MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *      MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
      *      CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                         MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                   ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 start
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
             READ  S138TMP-FILE   NEXT
                 AT  END
                     MOVE  1  TO  FLG-PRINT
                 NOT AT END
                     ADD   1   TO    CNT-READ
             END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE   WRK-PARA-HOSPID       TO    RECE10-HOSPID.
           MOVE   WRK-PARA-HOSPNUM      TO    RECE10-HOSPNUM.
           MOVE   "0"                   TO    RECE10-XXSTHKNJANUM
           MOVE   "9999999"             TO    RECE10-XXEDHKNJANUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE   WRK-PARA-SRYYM        TO    RECE10-SKYYM.
      * 
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
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
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_seikyu"       TO  MCP-TABLE
           MOVE    "key4"                TO  MCP-PATHNAME
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
           IF      MCP-RC              =   ZERO
      *
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
      *=== テスト患者の判定修正 Uchida(2007/05/08)
      *         MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
      *         MOVE    RECE10-PTID         TO  PTINF-PTID
      *         MOVE    PTINF-REC           TO  MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    RECE10-HOSPID        TO  WRK-PARA-HOSPID
               MOVE    RECE10-HOSPNUM       TO  WRK-PARA-HOSPNUM
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
                        UNTIL    IDX5  >     4  OR
                        RECE10-KOHNUM(IDX5)  =  SPACE
                 IF    RECE10-KOHNUM(IDX5)  =  "091"
                   MOVE    "051"      TO      RECE10-KOHNUM(IDX5)
                 END-IF
               END-PERFORM
           ELSE
               MOVE    1               TO  FLG-END
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_seikyu"       TO  MCP-TABLE
           MOVE    "key4"                TO  MCP-PATHNAME
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
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
      ***07/09/20 nagaya 4.0対応 start
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
      ***07/09/20 nagaya 4.0対応 end
                                            MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                            MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                            SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
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
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
      *
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD START ----------------------------------------
      *****************************************************************
      *    患者保険情報マスター読込処理
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    SYU-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SYU-PTID            TO  PTHKN-PTID
      *
		   MOVE    SYU-SYUHKNNUM       TO  PTHKN-HKNNUM
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKSTYMD
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKEDYMD
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key9"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF      MCP-RC            =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
             ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTHKNINF
             INITIALIZE                PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴情報検索
      *****************************************************************
       920-DBSELECT3-SEC                 SECTION.
      *
           INITIALIZE                        JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM     TO   JYURRK-HOSPNUM
           MOVE    WRK-NYUGAIKBN        TO   JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD     TO   JYURRK-SRYYMD
           MOVE    WRK-PARA-PRTLYMD     TO   JYURRK-UPSRYYMD
           MOVE    JYURRK-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_jyurrk"         TO   MCP-TABLE
           MOVE    "key21"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key21"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
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
           MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key21"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       920-DBCLOSE3-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD END   ----------------------------------------
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  WRK-PARA-HOSPID      TO  HKNJA-HOSPID
           MOVE  WRK-PARA-HOSPNUM     TO  HKNJA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE  WRK-HKNJANUM         TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNJAINF-KEY"       TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_hknjainf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
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
           IF  MCP-RC  =  ZERO
             MOVE  "HKNJAINF-KEY"  TO  ORC-DBPATH
             PERFORM  925-HKNJAINF-READ-SEC
      *     ELSE
      *       DISPLAY  "HKNJAINF SELECT ERR" UPON CONSOLE
           END-IF
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNJAINF-KEY"   TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE  "tbl_hknjainf"   TO  MCP-TABLE
           MOVE  "key"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"      USING
      ***07/09/20 nagaya 4.0対応 end
                                  MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                  MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                  SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
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
           MOVE  "tbl_hknjainf" TO  MCP-TABLE
           MOVE  "key"          TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"    USING
      ***07/09/20 nagaya 4.0対応 end
                                MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                          ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
             MOVE  HKNJA-HKNJANAME   TO  WRK-HKNJANAME
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1              TO  FLG-HKNJAINF
             MOVE  WRK-HKNJANUM   TO  WRK-HKNJANAME
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
           IF  RECE10-RJNHKNNUM NOT = SPACE
             MOVE   SYU-KOH-COMPFTN(1) TO  WRK-ZENCOMPFTN
           ELSE
             MOVE   SYU-SYUCOMPFTN  TO  WRK-ZENCOMPFTN
           END-IF
      ***  20050907 MOTOHASI 公費助成額の算出を修正
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF    SYU-KOH-HKNNUM(IDX) NOT  =  "027"
      *    差額から助成額(公費請求額)を算出
               IF    RECE10-RJNHKNNUM  NOT = SPACE
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
                     MOVE  SYU-KOH-HKNNUM(IDX)   TO  WRK-KOHNUM
      *    限度なし
                     PERFORM   950-HKNNUM-SET-SEC
      *--- 2009/07/01 DELETE START -------------------------------------
      *               IF   HKN-RECEFTNDSPKBN  =  1
      *     DISPLAY "四捨五入する" 
      *                 COMPUTE  WRK-KOHSEIKYUGK  =  SYU-SYUCOMPFTN
      *                                           -  SYU-KOH-COMPFTN(IDX)
      *                 COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
      *                                              -  SYU-SKJ-FTN(IDX)
      *               ELSE
      *--- 2009/07/01 DELETE END ---------------------------------------
                       COMPUTE  WRK-KOHSEIKYUGK = SYU-SYUCOMPFTN-ENTANI
                                          -  SYU-KOH-COMPFTN-ENTANI(IDX)
                       COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                    -  SYU-SKJ-FTN(IDX)
      *--- 2009/07/01 DELETE START -------------------------------------
      *               END-IF
      *--- 2009/07/01 DELETE END ---------------------------------------
                   ELSE
      *    限度あり
                     ADD    SYU-KOH-COMPFTN(IDX)  TO    WRK-KOHFTN
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
      *--- (2008/12/12) line add start monma ---------------------------
           COMPUTE  WRK-JIHIGK-G  =  WRK-JIHIGK-G
                                  +  SYU-JIHI-TOTAL
                                  +  SYU-JIHI-TOTAL-TAX
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *--- 2009/01/08 DELETE START -------------------------------------
      *     ADD    WRK-KOHSEIKYUGK  TO    WRK-KOHSEIKYUGK-G
      *--- 2009/01/08 DELETE END ---------------------------------------
           ADD    WRK-KOHSKJSEIKYUGK TO  WRK-KOHSEIKYUGK-G
           ADD    SYU-SKYMONEY     TO    WRK-MADOSEIKYUGK-G
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
             MOVE   RECE10-FTNMONEY(1)  TO   WRK-HKNFTN
      *--- 2009/01/08 ADD END ------------------------------------------
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  RECE10-HOSPID      TO  SYU-HOSPID
             MOVE  RECE10-HOSPNUM     TO  SYU-HOSPNUM
      ***07/09/20 nagaya 4.0対応 start
             MOVE  RECE10-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE10-PTID        TO  SYU-PTID
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "SYUNOU-KEY2"      TO  ORC-DBPATH
      *       CALL  "ORCMCPSUB"        USING
             MOVE    "tbl_syunou"     TO  MCP-TABLE
             MOVE    "key2"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
             IF  MCP-RC  =  ZERO
               MOVE  "SYUNOU-KEY2"  TO  ORC-DBPATH
               PERFORM  925-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
      *--- 2009/06/26 ADD START ----------------------------------------
               IF       SYU-TOTAL-HKNTEN =  0
                 AND    SYU-TOTAL-MONEY             =  0
                 AND  ( SYU-JIHI-TOTAL         NOT  =  0
                 OR     SYU-JIHI-TOTAL-TAX     NOT  =  0 
                 OR     SYU-TOTAL-TGMONEY      NOT  =  0
                 OR     SYU-TOTAL-TGMONEY-TAX  NOT  =  0 )
                 CONTINUE
               ELSE
      *--- 2009/06/26 ADD START ----------------------------------------
                 IF  RECE10-SRYYM  =  SYU-SRYYMD(1:6)
                     AND  SYU-DENPJTIKBN   NOT = "3"
                     AND  RECE10-TEKSTYMD  <=  SYU-SRYYMD
      *
      ***  20050412  @@@@-MOTOHASI  
                     AND  SYU-SYUHKNNUM  =  RECE10-HKNNUM
      ***  20050412  @@@@-MOTOHASI  
      *
      *    旧総合病院の時は該当する診療科のみ対象にする
      *--- 08/10/30  CHANGE  START ------------------------------------
      * 20081014 主科設定時に診療科を印字するSTART
                     IF    SYS-1001-HOSPSBT1 NOT =  1  OR
                           RECE10-NYUGAIKBN      = "1" OR
      *               IF     RECE10-NYUGAIKBN      = "1" OR
      * 20081014 主科設定時に診療科を印字するEND
      *--- 08/10/30  CHANGE  END ------------------------------------
                           RECE10-RECEKA         = SYU-SRYKA
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                 END-IF
      *--- 2009/06/26 ADD START ----------------------------------------
               END-IF
      *--- 2009/06/26 ADD START ----------------------------------------
      *
                   MOVE  "SYUNOU-KEY2"  TO  ORC-DBPATH
                   PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
      *      ELSE
      *         DISPLAY  "SYUNOU SELECT ERR" UPON CONSOLE
             END-IF
      *
             INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *       MOVE  "DBCLOSE"      TO  MCP-FUNC
             MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "SYUNOU-KEY2"  TO  ORC-DBPATH
      *       CALL  "ORCMCPSUB"  USING
             MOVE  "tbl_syunou" TO  MCP-TABLE
             MOVE  "key2"       TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"  USING
      ***07/09/20 nagaya 4.0対応 end
                                MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                          ORCMCPAREA
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL  "ORCMCPSUB"  USING
           MOVE  "tbl_syunou"  TO  MCP-TABLE
           MOVE  "key2"        TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"   USING
      ***07/09/20 nagaya 4.0対応 end
                              MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                        ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                              MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                              SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SYUNOU-REC
             MOVE  ZERO         TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1  TO  FLG-SYUNOU
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
           INITIALIZE                  SYUNOU-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  JYURRK-HOSPID        TO  SYU-HOSPID
           MOVE  JYURRK-HOSPNUM       TO  SYU-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE  JYURRK-NYUGAIKBN     TO  SYU-NYUGAIKBN
           MOVE  JYURRK-PTID          TO  SYU-PTID
           MOVE  JYURRK-DENPNUM       TO  SYU-DENPNUM
           MOVE  SYUNOU-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "SYUNOU-KEY"      TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE  "tbl_syunou"         TO  MCP-TABLE
           MOVE  "key"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
           IF  MCP-RC  =  ZERO
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "SYUNOU-KEY"  TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
             MOVE  "DBFETCH"  TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *       CALL  "ORCMCPSUB"  USING
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  "key"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                          ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 edn
                                MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC  TO  SYUNOU-REC
               MOVE  ZERO         TO  FLG-SYUNOU
             ELSE
               INITIALIZE  SYUNOU-REC
               MOVE  1            TO  FLG-SYUNOU
             END-IF

           ELSE
             MOVE    1            TO  FLG-SYUNOU
           END-IF
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "SYUNOU-KEY"  TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE  "tbl_syunou"     TO  MCP-TABLE
           MOVE  "key"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"      USING
      ***07/09/20 nagaya 4.0対応 end
                                  MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                  MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                  SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
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
           MOVE    JYURRK-HOSPNUM       TO  SYU-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    JYURRK-PTID          TO  SYU-PTID
           MOVE    JYURRK-NYUGAIKBN     TO  SYU-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD     TO  SYU-SKYSTYMD
           MOVE    WRK-PARA-PRTLYMD     TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY5"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
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
           IF      MCP-RC           =   ZERO
                   PERFORM 940-SYUNOU-FETCH-SEC
           ELSE
                   MOVE   1               TO  FLG-SYUNOU
           END-IF
           .
       940-SYUNOU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    収納情報１件読み込み
      *****************************************************************
       940-SYUNOU-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY5"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
           IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYUNOU-REC
                   MOVE    ZERO            TO  FLG-SYUNOU
                   IF    SYU-DENPJTIKBN    =   "3"
                     GO    TO    940-SYUNOU-FETCH-SEC
                   END-IF
      *
                   IF    SYU-SYUHKNNUM(1:2) NOT  =  "97" AND "98"
                     GO    TO    940-SYUNOU-FETCH-SEC
                   END-IF
      *
           ELSE
                   MOVE    1               TO  FLG-SYUNOU
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
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY5"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
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
      *--- 2009/06/26 CHANGE START -------------------------------------
      *     MOVE  JYURRK-HOSPNUM         TO  COMB-HOSPNUM
      ****07/09/20 nagaya 4.0対応 end
      *     MOVE  JYURRK-PTID            TO  COMB-PTID
      *     MOVE  JYURRK-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE  SYU-HOSPNUM          TO  COMB-HOSPNUM
           MOVE  SYU-PTID             TO  COMB-PTID
           MOVE  SYU-HKNCOMBINUM      TO  COMB-HKNCOMBINUM
      *--- 2009/06/26 CHANGE END   -------------------------------------
           MOVE  HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 en
           IF  MCP-RC  =  ZERO
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "HKNCOMBI-KEY"  TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
             MOVE  "DBFETCH"  TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *       CALL  "ORCMCPSUB"  USING
             MOVE  "tbl_hkncombi" TO  MCP-TABLE
             MOVE  "key"          TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"  USING
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
               MOVE  MCPDATA-REC  TO  HKNCOMBI-REC
               MOVE  ZERO         TO  FLG-HKNCOMBI
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE  1            TO  FLG-HKNCOMBI
             END-IF

           ELSE
             MOVE    1            TO  FLG-HKNCOMBI
           END-IF
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNCOMBI-KEY"  TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE  "tbl_hkncombi"   TO  MCP-TABLE
           MOVE  "key"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"      USING
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
           MOVE  WRK-PARA-HOSPNUM     TO  HKN-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE  WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE  HKNNUM-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNNUM-KEY2"        TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE  "tbl_hknnum"          TO  MCP-TABLE
           MOVE  "key2"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
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
             MOVE  "HKNNUM-KEY2"      TO  ORC-DBPATH
             PERFORM  955-HKNNUM-READ-SEC
      * 20080526 古い公費名を印字してしまう不具合を修正START tanaka
           ELSE
             MOVE    1    TO    FLG-HKNNUM
      * 20080526 古い公費名を印字してしまう不具合を修正END
      *       DISPLAY  "HKNNUM SELECT ERR" UPON CONSOLE
           END-IF
      *
      * 20080526 古い公費名を印字してしまう不具合を修正START tanaka
           PERFORM UNTIL       FLG-HKNNUM      =   1
             IF      HKN-TEKSTYMD            <=  WRK-SRYYMD
             AND     HKN-TEKEDYMD            >=  WRK-SRYYMD
               MOVE  1                  TO  FLG-HKNNUM   
             END-IF
      *
             IF      FLG-HKNNUM           =   ZERO
               MOVE  "tbl_hknnum"       TO  MCP-TABLE
               MOVE  "key2"             TO  MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
             END-IF
           END-PERFORM      
      * 20080526 古い公費名を印字してしまう不具合を修正END
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNNUM-KEY2"    TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE  "tbl_hknnum"       TO  MCP-TABLE
           MOVE  "key2"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"       USING
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL  "ORCMCPSUB"  USING
           MOVE  "tbl_hknnum" TO  MCP-TABLE
           MOVE  "key2"       TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"  USING
      ***07/09/20 nagaya 4.0対応 end
                              MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                        ORCMCPAREA
      ***07/09/20 nagaya 4.0対応 end
                              MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                              SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1  TO  FLG-HKNNUM
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
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    S100LNK-JOBID     TO  JOB-JOBID
           MOVE    S100LNK-SHELLID   TO  JOB-SHELLID
           MOVE    CNT-PAGE          TO  JOB-UPDCNT                        
           CALL    "ORCSJOB"       USING
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
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
           MOVE    "DBSTART"           TO  MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
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
