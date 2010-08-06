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
       PROGRAM-ID.             A00000S140.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保険別総括表
      *  管理者            :
      *  作成日付    作業者        記述
      *  08/04/23    @@@@-田中     新規作成
      *  08/06/17    @@@@-田中     処理速度の向上の修正
      *  08/10/15    @@@@-田中     主科設定時に診療科を印字する
      *                            市町村ごとの集計を追加
      *  08/10/23    @@@@-田中     保険別集計チェック表が印刷されない不具合を修正
      *  08/10/30    @@@@-田中     窓口請求額の不具合を修正
      *  08/12/12    @@@@-門間     自費(095XXXXX、096XXXXXで入力)の集計欄を追加
      *                            食事療養の収益を医療収益に加算
      *  09/01/08    @@@@-田中     医療収益の計算式を変更
      *  09/01/08    @@@@-田中     公費請求額の計算式を変更
      *  09/06/26    @@@@-門間     自費分が正しく集計されない不具合を修正
      *  09/07/01    @@@@-田中     公費請求額の端数処理を修正
      *  09/07/29    @@@@-田中     保険請求額の計算式を修正
      *  09/08/18    @@@@-田中     公費請求額の計算式を修正
      *  09/08/20    @@@@-田中     食事療養費の回数を5桁に変更
      *  09/11/06    @@@@-田中     同保険者番号に変更時の不具合を修正
      *  09/11/19    @@@@-田中     保険者一覧に国保が印字される不具合を修正
      *  09/11/19    @@@@-田中     保険者一覧を保険者番号順に印字する
      *  10/01/13    @@@@-田中     公費請求額の計算式を修正
      *  10/04/06    @@@@-田中     保険請求額の計算式を修正
      *  10/07/27    @@@@-門間     自費の集計処理を修正
      *  10/08/04    @@@@-田中     公費請求額の条件を追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    後期高齢用ファイル
           SELECT  S140TMP-FILE    ASSIGN    S140TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  S140TMP-KEY
                                   FILE    STATUS  IS  STS-S140TMP.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    後期高齢一時ファイル
       FD  S140TMP-FILE.
           COPY    "A00000S140TMP.INC".
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
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
      *    後期高齢一時ファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //S140TP//.
      *
      * 20081014 市町村ごとの集計を追加START
      *     COPY    "A00000S140G.INC".
      *     COPY    "A00000S140N.INC".
           COPY    "A00000S140G1.INC".
           COPY    "A00000S140N1.INC".
           COPY    "A00000S140G2.INC".
           COPY    "A00000S140N2.INC".
      * 20081014 市町村ごとの集計を追加END
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-RECE            PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-S140TMP         PIC X(02).
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
           03  FLG-S140TMP              PIC 9(01).
           03  FLG-PRINT                PIC 9(01).
           03  INP-STS                  PIC 9(04).
           03  FLG-KUMIAI               PIC 9(01).
           03  FLG-HKNNUM               PIC 9(01).
           03  FLG-PTHKNINF             PIC 9(01).
           03  FLG-HEIYOU               PIC 9(01).
           03  FLG-RJNHKN               PIC 9(01).
           03  FLG-END                  PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
      *--- 2009/07/29 ADD START ----------------------------------------
           03  FLG-TYOKI                PIC 9(01).
      *--- 2009/07/29 ADD END ------------------------------------------
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-KOHI            PIC 9(01).
           03  CNT-NINZU           PIC 9(06).
           03  CNT-JYURRK          PIC 9(06).
           03  CNT-DATA            PIC 9(06).
           03  CNT-READ            PIC 9(06).
           03  CNT-KOH             PIC 9(04).
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
           03  IDX                 PIC 9(01).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(03).
      *--- 2009/07/29 ADD START ----------------------------------------
           03  IDX7                PIC 9(01).
      *--- 2009/07/29 ADD END ------------------------------------------
           03  IDA-G.
               05  IDA             PIC 9(02).
           03  IDB-G.
               05  IDB             PIC 9(02).
           03  IDC-G.
               05  IDC             PIC 9(04).
           03  IDD-G.
               05  IDD             PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).

      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID            PIC 9(07).
           03  WRK-PARA-SHELLID          PIC X(08).
           03  WRK-PARA-HOSPNUM          PIC 9(02).
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
           03  WRK-HKNJANUM              PIC X(08).
      * 20081014 市町村ごとの集計を追加START
      *     03  WRK-HKNJANAME         PIC X(40).
           03  WRK-HKNJANAME             PIC X(50).
      * 20081014 市町村ごとの集計を追加END
           03  WRK-KYURATE               PIC 9(02).
           03  WRK-KASAN-TBL.
               05  WRK-KENSU             PIC 9(04).
               05  WRK-NISSU             PIC 9(04).
               05  WRK-TENSU             PIC 9(09).
      *--- 2009/01/08 CHANGE START -------------------------------------
      *         05  WRK-MADOGK            PIC 9(09).
               05  WRK-MADOGK            PIC S9(09).
      *--- 2009/01/08 CHANGE END ---------------------------------------
      *--- (2008/12/12) line add start monma ---------------------------
               05  WRK-JIHIGK            PIC 9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
               05  WRK-HKNGK             PIC 9(09).
      *--- 2010/04/06 ADD START ----------------------------------------
               05  WRK-HKNGK2            PIC 9(09).
      *--- 2010/04/06 ADD END ------------------------------------------
      *--- 2010/01/13 CHANGE START -------------------------------------
      *         05  WRK-KOHGK             PIC 9(09).
               05  WRK-KOHGK             PIC S9(09).
      *--- 2010/01/13 CHANGE END ---------------------------------------
               05  WRK-SYUEKI            PIC 9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *         05  WRK-SNISSU            PIC 9(04).
               05  WRK-SNISSU            PIC 9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
               05  WRK-SSEIKYU           PIC 9(09).
               05  WRK-SFTNGK            PIC 9(09).
               05  WRK-KKENSU            PIC 9(04).
               05  WRK-KNISSU            PIC 9(04).
               05  WRK-KTENSU            PIC 9(09).
           03  WRK-KOHI-GOKEI-TBL.
               05  WRK-GKKENSU           PIC 9(04).
               05  WRK-GKNISSU           PIC 9(04).
               05  WRK-GKTENSU           PIC 9(09).
      *--- 2009/01/08 CHANGE START -------------------------------------
      *     03  WRK-MADOSEIKYUGK-G        PIC 9(09).
           03  WRK-MADOSEIKYUGK-G        PIC S9(09).
      *--- 2009/01/08 CHANGE END ---------------------------------------
      *--- (2008/12/12) line add start monma ---------------------------
           03  WRK-JIHIGK-G              PIC 9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
      *--- 2009/08/18 CHANGE START -------------------------------------
      *     03  WRK-KOHSEIKYUGK          PIC 9(09).
           03  WRK-KOHSEIKYUGK           PIC S9(09).
      *--- 2009/08/18 CHANGE END ---------------------------------------
      *--- 2010/01/13 CHANGE START -------------------------------------
      *     03  WRK-KOHSEIKYUGK-G         PIC 9(09).
           03  WRK-KOHSEIKYUGK-G         PIC S9(09).
      *--- 2010/01/13 CHANGE END ---------------------------------------
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
      * 20081014 市町村ごとの集計を追加START
      *     03  WRK-CNT1                 PIC 9(02).
      *     03  WRK-CNT2                 PIC 9(02).
      *     03  WRK-CNT3                 PIC 9(02).
      *     03  WRK-CNT4                 PIC 9(02).
           03  WRK-CNT1                  PIC 9(03).
           03  WRK-CNT2                  PIC 9(03).
           03  WRK-CNT3                  PIC 9(03).
           03  WRK-CNT4                  PIC 9(03).
      * 20081014 市町村ごとの集計を追加end
           03  WRK-KOHNAME-G             PIC X(24).
           03  WRK-KOHNAME-G2            PIC X(24).
      *--- 2009/06/26 ADD START ----------------------------------------
           03  WRK-JIHI                  PIC 9(07).
           03  WRK-TGMONEY               PIC 9(07).
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
      *--- 2009/01/08 CHANGE START -------------------------------------
           03  WRK-Z93                   PIC --------Z.
      *--- 2009/01/08 CHANGE END ---------------------------------------
      *--- 2009/08/20 ADD START ----------------------------------------
           03  WRK-Z52                   PIC ZZZZZ.
      *--- 2009/08/20 ADD END ------------------------------------------
      *
           03  WRK-SYS-BEDSU             PIC 9(04).
           03  WRK-SYS-BEDSUIPN          PIC 9(04).
      *--- 2009/01/08 ADD START ----------------------------------------
           03  WRK-FTNFLG               PIC 9(01).
           03  WRK-HKNFTN               PIC 9(09).
           03  WRK-KOHFTN               PIC 9(09).
      *--- 2009/01/08 ADD END ------------------------------------------
      *--- 2009/07/29 ADD START ----------------------------------------
           03  WRK-HKNSKYMONEY          PIC 9(09).
      *--- 2009/01/08 DELETE START -------------------------------------
      *     03  WRK-MSKYMONEY            PIC 9(09).
      *--- 2009/01/08 DELETE END ---------------------------------------
           03  WRK-TYOKIFTN             PIC 9(09).
      *--- 2009/07/29 ADD END ------------------------------------------
      *--- 2009/11/19 ADD START ----------------------------------------
           03  IDX-S                     PIC 9(03).
           03  IDX-N                     PIC 9(03).
           03  WRK-HKNJA-TBL             PIC X(100).
      *--- 2009/11/19 ADD END ------------------------------------------
      *--- 2010/04/05 ADD START ----------------------------------------
           03  WRK-WARI                  PIC 9(02)V9.
      *--- 2010/04/05 ADD END ------------------------------------------
      *
      *    診療科名退避エリア
       01  SUM-AREA.
           03  SUM-SRYKAMAX              PIC 9(03).
      * 20081014 市町村ごとの集計を追加START
      *     03  SUM-SRYDR-TBL       OCCURS 100.
           03  SUM-SRYDR-TBL       OCCURS 300.
      * 20081014 市町村ごとの集計を追加END
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
           COPY    "CPSHELLTBL.INC".
      *
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
      *
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
           COPY    "CPSK5113.INC".
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
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
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
           INITIALIZE                  SPA-AREA
      *
           MOVE    S100LNK-HOSPNUM    TO   WRK-PARA-HOSPNUM
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      *    ステップ管理開始処理
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                 JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
           MOVE    S100LNK-JOBID      TO   JOB-JOBID
           MOVE    S100LNK-SHELLID    TO   JOB-SHELLID
           MOVE    "A00000S100"       TO   JOB-PGID
           MOVE    "保険別請求チェック表(後期高齢者)"
                                      TO   JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
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
           MOVE   S100LNK-PRTKANRI-SKYYMD   TO   WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE   WRK-HENYMDG               TO   WRK-SEIYMD
      *
           MOVE   S100LNK-NYUGAIKBN         TO   WRK-PARA-NYUGAIKBN
           MOVE   S100LNK-PRTKANRI-SKYYMD   TO   SPA-SYSYMD
      *
      *
      *    対象日付処理
           MOVE    S100LNK-YM               TO   WRK-SRYYMD (1:6)
           MOVE    "01"                     TO   WRK-SRYYMD (7:2)
           MOVE    S100LNK-YM               TO   WRK-SYMD (1:6)
           MOVE    "01"                     TO   WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)        TO   WRK-SRYYM
           MOVE    S100LNK-YM               TO   WRK-PARA-SRYYM
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    S100LNK-YM               TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD                 TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    S100LNK-YM               TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                     TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集^M
           MOVE    SPACE                    TO   SYS-1001-REC
           INITIALIZE                            SYS-1001-REC
           MOVE    "1001"                   TO   SYS-1001-KANRICD
           MOVE    "*"                      TO   SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM              TO   SYS-1001-HOSPNUM
           MOVE    SPA-SYSYMD               TO   SYS-1001-STYUKYMD
                                                 SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC             TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
             MOVE    MCPDATA-REC            TO   SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM       TO   WRK-PARA-HOSPNUM
             MOVE    SYS-1001-BEDSU         TO   WRK-SYS-BEDSU
             MOVE    SYS-1001-BEDSUIPN      TO   WRK-SYS-BEDSUIPN
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO   WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      **
      *    自賠責点数単価取得
           MOVE   SPACE                     TO   SYS-4001-REC
           INITIALIZE                       SYS-4001-REC
           MOVE   "4001"                    TO   SYS-4001-KANRICD
           MOVE   "*"                       TO   SYS-4001-KBNCD
           MOVE   SPA-HOSPNUM               TO   SYS-4001-HOSPNUM
           MOVE   SPA-SYSYMD                TO   SYS-4001-STYUKYMD
                                                 SYS-4001-EDYUKYMD
           MOVE   SYS-4001-REC              TO   MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC     TO   SYS-4001-REC
           ELSE
             INITIALIZE                   SYS-4001-REC
           END-IF
           IF    SYS-4001-TENKBN   =   "1"
             MOVE    11.5                   TO   WRK-JIBAITNK
           ELSE
             MOVE    12                     TO   WRK-JIBAITNK
           END-IF
      *
      *    診療科情報取得
           MOVE    SPACE                    TO   SYS-1005-REC
           INITIALIZE                            SYS-1005-REC
           MOVE    "1005"                   TO   SYS-1005-KANRICD
           MOVE    SPA-SYSYMD               TO   SYS-1005-STYUKYMD
                                                 SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM              TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD         TO   WRK-SYMD
           PERFORM  820-SYSKANRI-DBSELECT-SEC
           MOVE     1                       TO   IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3  =  1
             IF  FLG-SYSKANRI3 = ZERO
               MOVE  SYS-1005-KBNCD         TO   SUM-SRYKA(IDX1)
               MOVE  SYS-1005-SRYKANAME1    TO   SUM-SRYKANAME(IDX1)
               ADD     1                    TO   IDX1
             END-IF
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           SUBTRACT    1        FROM  IDX1
           MOVE  IDX1           TO   SUM-SRYKAMAX
           PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *
      *
           MOVE    "S140TP"                TO   S140TP-FILE-ID
           MOVE    S100LNK-PRTKANRI-TERMID TO   S140TP-TERMID
           MOVE    WRK-PARA-HOSPNUM        TO   S140TP-HOSPNUM
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
           OPEN     OUTPUT  S140TMP-FILE
           CLOSE            S140TMP-FILE
           OPEN     I-O     S140TMP-FILE
      *--- 2009/06/26 ADD START ----------------------------------------
      *    自費データ集計
           IF  WRK-PARA-NYUGAIKBN  =  2
             OR  WRK-PARA-NYUGAIKBN  =  SPACE
             PERFORM  2001-JIHI-SYUKEI-SEC
           END-IF
      *--- 2009/06/26 ADD END   ----------------------------------------
      *    後期高齢データ集計
           PERFORM  2001-KOKUHO-SYUKEI-SEC
      *
           CLOSE   S140TMP-FILE
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
           IF  SYU-SYUHKNNUM  =  "039"  OR  "040"
             IF       SYU-TOTAL-HKNTEN            =  0
               AND    SYU-TOTAL-MONEY             =  0
               AND  ( SYU-JIHI-TOTAL         NOT  =  0
               OR     SYU-JIHI-TOTAL-TAX     NOT  =  0
               OR     SYU-TOTAL-TGMONEY      NOT  =  0
               OR     SYU-TOTAL-TGMONEY-TAX  NOT  =  0 )
      *--- 2010/07/27 DELETE START -------------------------------------
      *         COMPUTE  WRK-JIHI     =  WRK-JIHI
      *                               +  SYU-JIHI-TOTAL
      *                               +  SYU-JIHI-TOTAL-TAX
      *         COMPUTE  WRK-TGMONEY  =  WRK-TGMONEY
      *                               +  SYU-TOTAL-TGMONEY
      *                               +  SYU-TOTAL-TGMONEY-TAX
      *--- 2010/07/27 DELETE END   -------------------------------------
               PERFORM  2001-KASAN-JIHI-G-SEC
      *--- 2010/07/27 DELETE START -------------------------------------
      *         MOVE     0   TO   WRK-JIHI
      *                           WRK-TGMONEY
      *--- 2010/07/27 DELETE END   -------------------------------------
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
      *    キー設定
           INITIALIZE    S140TMP-REC
      *
           MOVE     SYU-NYUGAIKBN       TO    S140TMP-NYUGAIKBN
      *
           IF    SYS-1001-HOSPSBT1  =  1
             OR  SYS-2009-SYUKA-KBN = 1
             MOVE    SYU-SRYKA          TO    S140TMP-SRYKA
           ELSE
             MOVE    "00"               TO    S140TMP-SRYKA
           END-IF
      *
      *    TMPファイル読み込み
           MOVE    2         TO   FLG-TYPE
           PERFORM  800-SKTTMP-READ-SEC
      *
           MOVE   PTHKN-HKNJANUM   TO    WRK-HKNJANUM
      *
      *    公費設定
           MOVE   SYU-KOH1HKNNUM   TO   WRK-JIHI-HKNNUM(1)
           MOVE   SYU-KOH2HKNNUM   TO   WRK-JIHI-HKNNUM(2)
           MOVE   SYU-KOH3HKNNUM   TO   WRK-JIHI-HKNNUM(3)
           MOVE   SYU-KOH4HKNNUM   TO   WRK-JIHI-HKNNUM(4)
      *
      *    公費集計処理
           IF    SYU-KOH1HKNNUM  NOT =  SPACE
             PERFORM  2001-JIHI-K-DATA-SEC
           END-IF
      *
      *    明細集計場所設定
           PERFORM  2001-JIHI-IDX-SET-SEC
      *    社保データ集計
           PERFORM  2001-JIHI-DATA-SET-SEC
      *
      *    書き出し
           IF  FLG-S140TMP   =  1
             ADD      1             TO   CNT-DATA
             WRITE    S140TMP-REC
           ELSE
             REWRITE  S140TMP-REC
           END-IF
      *
           .
        2001-KASAN-JIHI-G-EXT.
           EXIT.
      *****************************************************************
      *    自費集計場所設定
      *****************************************************************
       2001-JIHI-IDX-SET-SEC                SECTION.
      *
      *    一般後期高齢
           IF  ( SYU-SYUHKNNUM   =  "039"
             OR  SYU-SYUHKNNUM   =  "040" )
             IF  PTHKN-HOJOKBN   =  "1"
               MOVE       1      TO    IDX1
             ELSE
               MOVE       2      TO    IDX1
             END-IF
      *    小計集計場所設定
             MOVE       3      TO    IDX2
           END-IF
      *
           .
       2001-JIHI-IDX-SET-EXT.
           EXIT.
      *****************************************************************
      *    自費集計処理２
      *****************************************************************
       2001-JIHI-DATA-SET-SEC                SECTION.
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
      *--- 2010/07/27 DELETE START -------------------------------------
      *     ADD     WRK-TGMONEY      TO     S140TMP-MADOGK(IDX1)
      *                                     S140TMP-MADOGK(IDX2)
      *                                     S140TMP-GMADOGK
      *--- 2010/07/27 DELETE END   -------------------------------------
      *
      *    自費金額加算
      *--- 2010/07/27 CHANGE START -------------------------------------
      *     ADD     WRK-JIHI         TO     S140TMP-JIHIGK(IDX1)
           ADD     SYU-SKYMONEY     TO     S140TMP-JIHIGK(IDX1)
      *--- 2010/07/27 CHANGE END   -------------------------------------
                                           S140TMP-JIHIGK(IDX2)
                                           S140TMP-GJIHIGK
      *
      *    医療収益
      *--- 2010/07/27 DEL&CHA START ------------------------------------
      *     ADD     WRK-JIHI         TO     S140TMP-SYUEKI(IDX1)
      *                                     S140TMP-SYUEKI(IDX2)
      *                                     S140TMP-GSYUEKI
      *     ADD     WRK-TGMONEY      TO     S140TMP-SYUEKI(IDX1)
           ADD     SYU-SKYMONEY      TO     S140TMP-SYUEKI(IDX1)
      *--- 2010/07/27 DEL&CHA END   ------------------------------------
                                            S140TMP-SYUEKI(IDX2)
                                            S140TMP-GSYUEKI
      *
           MOVE    1                  TO   IDC
           MOVE    300                TO   IDX-MAX
           PERFORM    UNTIL   IDC   >    IDX-MAX
             IF   S140TMP-HKNJANUM(IDC)   =  SPACE  OR  WRK-HKNJANUM
               MOVE    WRK-HKNJANUM   TO   S140TMP-HKNJANUM(IDC)
               MOVE    ZERO           TO   IDX-MAX
             ELSE
               ADD     1              TO   IDC
             END-IF
           END-PERFORM
      *
      *    保険者加算
           IF    IDC    <=  300
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
      *--- 2010/07/27 DELETE START -------------------------------------
      *       ADD    WRK-TGMONEY          TO    S140TMP-HMADOGK(IDC)
      *--- 2010/07/27 DELETE END   -------------------------------------
      *
      *    自費計加算
      *--- 2010/07/27 CHANGE START -------------------------------------
      *       ADD    WRK-JIHI             TO    S140TMP-HJIHIGK(IDC)
             ADD    SYU-SKYMONEY         TO    S140TMP-HJIHIGK(IDC)
      *--- 2010/07/27 CHANGE END   -------------------------------------
      *
      *    医療収益
      *--- 2010/07/27 DEL&CHA START ------------------------------------
      *       ADD    WRK-JIHI             TO    S140TMP-HSYUEKI(IDC)
      *       ADD    WRK-TGMONEY          TO    S140TMP-HSYUEKI(IDC)
             ADD    SYU-SKYMONEY         TO    S140TMP-HSYUEKI(IDC)
      *--- 2010/07/27 DEL&CHA END   ------------------------------------
      *
           END-IF
      *
           .
       2001-JIHI-DATA-SET-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢公費集計処理
      *****************************************************************
       2001-JIHI-K-DATA-SEC                SECTION.
      *
           PERFORM VARYING IDX5   FROM 1  BY  1
                   UNTIL   IDX5  >   4
                   OR  WRK-JIHI-TBL(IDX5)   =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     300
                      OR       S140TMP-KOH1-KEY (IDX4) = SPACE
               IF  WRK-JIHI-TBL (IDX5)  =  S140TMP-KOHNUM (IDX4)
                 MOVE    IDX4      TO   CNT-KOH
                 MOVE    300       TO   IDX4
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
             IF  IDX4  < 300
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    公費名
      *    ０９１と０５１を同じ公費として扱う
             IF   WRK-JIHI-TBL(IDX5)  =  "091"
               MOVE  "051"               TO   S140TMP-KOHNUM(CNT-KOH)
             ELSE
               MOVE  WRK-JIHI-TBL(IDX5)  TO   S140TMP-KOHNUM(CNT-KOH)
             END-IF
      *    自費・保険外のみの場合、件数・日数は集計しない
      *
           END-PERFORM
           .
       2001-DATA-SET21-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD END   ----------------------------------------
      *****************************************************************
      *    後期高齢データ抽出処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC                SECTION.
      *
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *
             IF  (RECE10-HKNNUM  =  "039"
                 OR  RECE10-HKNNUM  =  "040")
                 AND ((( RECE10-SRYYM   =   WRK-PARA-SRYYM)
                 AND (  RECE10-SKYKBN  NOT =  "0")
                 AND (( RECE10-SKYYM   =   "000000" )
                 OR  (  RECE10-SKYYM   =   WRK-PARA-SRYYM)))
                 OR  (( RECE10-SKYYM   =   WRK-PARA-SRYYM)
                 AND (  RECE10-SKYKBN   NOT =  "0")))
      *    集計場所設定
               PERFORM  2001-IDX-SET2-SEC
      *
      *    後期高齢データ集計
               PERFORM  2001-DATA-SET2-SEC
             END-IF
      *
             PERFORM  900-SEIKYU-READ-SEC
           END-PERFORM
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
      *
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢集計場所設定
      *****************************************************************
        2001-IDX-SET2-SEC                SECTION.
      *
      *    一般後期高齢
           IF  (RECE10-HKNNUM  =  "039"
               OR    RECE10-HKNNUM  =  "040")
               IF    RECE10-KYURATE  =  9
                   MOVE          1      TO    IDX1
               ELSE
                   MOVE          2      TO    IDX1
               END-IF
      *    小計集計場所設定
               MOVE              3      TO    IDX2
           END-IF
      *
           .
       2001-IDX-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢者データ集計処理
      *****************************************************************
       2001-DATA-SET2-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S140TMP-REC
      *
           MOVE    RECE10-NYUGAIKBN    TO    S140TMP-NYUGAIKBN
           MOVE    RECE10-RECEKA       TO    S140TMP-SRYKA
      *    TMPファイル読み込み
           MOVE    2                   TO    FLG-TYPE
           PERFORM 800-SKTTMP-READ-SEC
      *
      *    加算データ設定
           PERFORM  2001-KASAN-SEC
      *
      *    件数加算
           ADD    1                    TO    S140TMP-KENSU(IDX1)
                                             S140TMP-KENSU(IDX2)
                                             S140TMP-GKENSU
      *
      *    日数加算
           ADD    WRK-NISSU            TO    S140TMP-NISSU(IDX1)
                                             S140TMP-NISSU(IDX2)
                                             S140TMP-GNISSU
      *
      *    点数加算
           ADD    WRK-TENSU            TO    S140TMP-TENSU(IDX1)
                                             S140TMP-TENSU(IDX2)
                                             S140TMP-GTENSU
      *
      *    窓口請求額加算
           ADD    WRK-MADOGK           TO    S140TMP-MADOGK(IDX1)
                                             S140TMP-MADOGK(IDX2)
                                             S140TMP-GMADOGK
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計
           ADD    WRK-JIHIGK           TO    S140TMP-JIHIGK(IDX1)
                                             S140TMP-JIHIGK(IDX2)
                                             S140TMP-GJIHIGK
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額加算
           ADD    WRK-HKNGK            TO    S140TMP-HKNGK(IDX1)
                                             S140TMP-HKNGK(IDX2)
                                             S140TMP-GHKNGK
      *
      *    公費請求額
           ADD    WRK-KOHGK            TO    S140TMP-KOHGK(IDX1)
                                             S140TMP-KOHGK(IDX2)
                                             S140TMP-GKOHGK
      *
      *    医療収益
           ADD    WRK-SYUEKI           TO    S140TMP-SYUEKI(IDX1)
                                             S140TMP-SYUEKI(IDX2)
                                             S140TMP-GSYUEKI
      *
      *
      *    日数加算
           ADD    WRK-SNISSU           TO    S140TMP-SNISSU(IDX1)
                                             S140TMP-SNISSU(IDX2)
                                             S140TMP-SGNISSU
      *    請求額加算
           ADD    WRK-SSEIKYU          TO    S140TMP-SSEIKYU(IDX1)
                                             S140TMP-SSEIKYU(IDX2)
                                             S140TMP-SGSEIKYU
      *    標準負担額加算
           ADD    WRK-SFTNGK           TO    S140TMP-SFTNGK(IDX1)
                                             S140TMP-SFTNGK(IDX2)
                                             S140TMP-SGFTNGK
      *
      *
      *    公費集計処理
           IF    RECE10-KOHNUM(1)  NOT =  SPACE
             PERFORM  2001-DATA-SET21-SEC
           END-IF
      *
      *    保険者毎の集計処理
      * 20081014 市町村ごとの集計を追加START
      *     IF    RECE10-HKNJANUM(7:2)  =  SPACE
      *       MOVE  RECE10-HKNJANUM(1:6)    TO    WRK-HKNJANUM
      *     ELSE
      *       MOVE  RECE10-HKNJANUM(3:6)    TO    WRK-HKNJANUM
             MOVE  RECE10-HKNJANUM   TO    WRK-HKNJANUM
      *     END-IF
      * 20081014 市町村ごとの集計を追加END
      *
           PERFORM  2001-DATA-SET22-SEC
      *
      *    書き出し
           IF  FLG-S140TMP   =  1
             ADD      1              TO    CNT-DATA
             WRITE    S140TMP-REC
           ELSE
             REWRITE  S140TMP-REC
           END-IF
      *
           .
       2001-DATA-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢公費集計処理
      *****************************************************************
       2001-DATA-SET21-SEC                SECTION.
      *
           PERFORM VARYING IDX5   FROM 1  BY  1
                   UNTIL   IDX5  >   4
                   OR  RECE10-KOHNUM(IDX5)   =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
      * 20081014 市町村ごとの集計を追加START
      *                UNTIL    IDX4  >     100
                      UNTIL    IDX4  >     300
      * 20081014 市町村ごとの集計を追加END
                      OR       S140TMP-KOH1-KEY (IDX4) = SPACE
               IF  RECE10-KOHNUM (IDX5)  =  S140TMP-KOHNUM (IDX4)
                   AND  RECE10-RJNHKNNUM  =  S140TMP-RJNKBN(IDX4)
                 MOVE    IDX4      TO   CNT-KOH
      * 20081014 市町村ごとの集計を追加START
      *             MOVE  100    TO  IDX4
                 MOVE    300       TO   IDX4
      * 20081014 市町村ごとの集計を追加END
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
      * 20081014 市町村ごとの集計を追加START
      *       IF  IDX4  < 100
             IF  IDX4  < 300
      * 20081014 市町村ごとの集計を追加END
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    老人併用区分設定
             MOVE  RECE10-RJNHKNNUM         TO   S140TMP-RJNKBN(CNT-KOH)
      *    公費名
             MOVE  RECE10-KOHNUM(IDX5)      TO   S140TMP-KOHNUM(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  S140TMP-KKENSU(CNT-KOH)
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX5 + 1)   TO   S140TMP-KNISSU(CNT-KOH)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX5 + 1) TO   S140TMP-KTENSU(CNT-KOH)
      *
             ADD     1                      TO   WRK-KKENSU
             ADD  RECE10-JNISSU(IDX5 + 1)   TO   WRK-KNISSU
             ADD  RECE10-TOTALTEN(IDX5 + 1) TO   WRK-KTENSU
      *
           END-PERFORM
           .
       2001-DATA-SET21-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢保険者毎の処理
      *****************************************************************
       2001-DATA-SET22-SEC                SECTION.
      *
           MOVE    1                  TO   IDC
      * 20081014 市町村ごとの集計を追加START
      *     MOVE    100            TO    IDX-MAX
           MOVE    300                TO   IDX-MAX
      * 20081014 市町村ごとの集計を追加END
           PERFORM    UNTIL   IDC   >    IDX-MAX
             IF   S140TMP-HKNJANUM(IDC)   =  SPACE  OR WRK-HKNJANUM
               MOVE    WRK-HKNJANUM   TO   S140TMP-HKNJANUM(IDC)
               MOVE    ZERO           TO   IDX-MAX
             ELSE
               ADD     1              TO   IDC
             END-IF
           END-PERFORM
      *
      *    保険者加算
      * 20081014 市町村ごとの集計を追加START
      *     IF    IDC    <=  100
           IF    IDC    <=  300
      * 20081014 市町村ごとの集計を追加END
      *    件数加算
             ADD    1                    TO    S140TMP-HKENSU(IDC)
      *
      *    日数加算
             ADD    WRK-NISSU            TO    S140TMP-HNISSU(IDC)
      *
      *    点数加算
             ADD    WRK-TENSU            TO    S140TMP-HTENSU(IDC)
      *
      *    窓口請求額加算
             ADD    WRK-MADOGK           TO    S140TMP-HMADOGK(IDC)
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計加算
             ADD    WRK-JIHIGK           TO    S140TMP-HJIHIGK(IDC)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額加算
             ADD    WRK-HKNGK            TO    S140TMP-HHKNGK(IDC)
      *
      *    公費請求額
             ADD    WRK-KOHGK            TO    S140TMP-HKOHGK(IDC)
      *
      *    医療収益
             ADD    WRK-SYUEKI           TO    S140TMP-HSYUEKI(IDC)
      *
      *
      *    日数加算
             ADD    WRK-SNISSU           TO    S140TMP-HSNISSU(IDC)
      *
      *    請求額加算
             ADD    WRK-SSEIKYU          TO    S140TMP-HSSEIKYU(IDC)
      *
      *    標準負担額加算
             ADD    WRK-SFTNGK           TO    S140TMP-HSFTNGK(IDC)
      *
      *
           END-IF
      *
      *
           .
       2001-DATA-SET22-EXT.
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
           MOVE    RECE10-JNISSU(1)       TO   WRK-NISSU
           MOVE    RECE10-TOTALTEN(1)     TO   WRK-TENSU
      *--- 2009/07/29 ADD START ----------------------------------------
           MOVE    ZERO                   TO   FLG-TYOKI
      *--- 2009/07/29 ADD END ------------------------------------------
      *
      *    限度額が有る場合は(点数×１０) − 患者負担 で算出する
           IF    RECE10-FTNMONEY(1)  =  ZERO
               AND  RECE10-KYURATE  NOT =  ZERO
      *--- 2009/07/29 ADD START ----------------------------------------
             IF   (RECE10-TOKKI(1)  =  "02"
                 OR  RECE10-TOKKI(2)  =  "02"
                 OR  RECE10-TOKKI(3)  =  "02"
                 OR  RECE10-TOKKI(4)  =  "02"
                 OR  RECE10-TOKKI(5)  =  "02")
               MOVE    1        TO   FLG-TYOKI
             ELSE
               IF  (RECE10-TOKKI(1)  =  "16"
                   OR  RECE10-TOKKI(2)  =  "16"
                   OR  RECE10-TOKKI(3)  =  "16"
                   OR  RECE10-TOKKI(4)  =  "16"
                   OR  RECE10-TOKKI(5)  =  "16")
                 MOVE    2        TO   FLG-TYOKI
               ELSE
      *--- 2009/07/29 ADD END ------------------------------------------
                 COMPUTE   WRK-HKNGK  =  RECE10-TOTALTEN(1)
                                      *  RECE10-KYURATE
      *--- 2009/07/29 ADD START ----------------------------------------
               END-IF
             END-IF
      *--- 2009/07/29 ADD END ------------------------------------------
           ELSE
             COMPUTE   WRK-HKNGK  =  (RECE10-TOTALTEN(1)  *  10)
                                  -  RECE10-FTNMONEY(1)
           END-IF

      *    収納情報から窓口請求額、公費請求額を集計
           MOVE    ZERO                     TO    WRK-MADOSEIKYUGK-G
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
               MOVE    RECE10-JNISSU(2)     TO   WRK-NISSU
               MOVE    RECE10-TOTALTEN(2)   TO   WRK-TENSU
               COMPUTE  WRK-HKNGK  =  (RECE10-TOTALTEN(2) * 10)
                                   +  RECE10-SHOKUJIRYOYOHI(2)
               ADD     WRK-HKNGK            TO   WRK-KOHGK
               MOVE    ZERO                 TO   WRK-HKNGK
           END-IF
      *
      *--- (2008/12/12) line del start monma ---------------------------
      *    医療収益
      *     COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
      *                          +  WRK-KOHGK
      *--- (2008/12/12) line del end   monma ---------------------------
      *
      *    日数
               MOVE  RECE10-SHOKUJINISSU(1)     TO   WRK-SNISSU
      *    請求額
               MOVE  RECE10-SHOKUJIRYOYOHI(1)   TO   WRK-SSEIKYU
      *    標準負担額
               MOVE  RECE10-SHOKUJIFTN(1)       TO   WRK-SFTNGK
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
      *    後期高齢データ印刷処理

           PERFORM  2002-PRINT-TYPE2-SEC
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢データ印刷処理
      *****************************************************************
       2002-PRINT-TYPE2-SEC                SECTION.
      *
           OPEN     INPUT        S140TMP-FILE
           MOVE     ZERO        TO   FLG-PRINT
           INITIALIZE                S140TMP-REC
      *
           MOVE    2            TO   FLG-TYPE
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT   =    1
      *
      *--- 2009/11/19 ADD START ----------------------------------------
      *    後期高齢者保険者並び替え処理
             IF   S140TMP-HKNJANUM(1)  NOT =  SPACE
               MOVE    1        TO   IDX-S
               MOVE    2        TO   IDX-N
               PERFORM  UNTIL  IDX-S  NOT <  300
                        OR  S140TMP-HKNJANUM(IDX-S)  =  SPACE
                 PERFORM  UNTIL  IDX-N  >  300
                          OR  S140TMP-HKNJANUM(IDX-N)  =  SPACE
                   IF  S140TMP-HKNJANUM(IDX-S) > S140TMP-HKNJANUM(IDX-N)
                     MOVE   S140TMP-HKNJA-TBL(IDX-N)  TO   WRK-HKNJA-TBL
                     MOVE   S140TMP-HKNJA-TBL(IDX-S)
                                           TO   S140TMP-HKNJA-TBL(IDX-N)
                     MOVE   WRK-HKNJA-TBL  TO   S140TMP-HKNJA-TBL(IDX-S)
                   END-IF
                   ADD    1      TO   IDX-N
                 END-PERFORM
      *    N番目の値を決定する為にインデックスの再設定
                 ADD    1        TO   IDX-S
                 COMPUTE  IDX-N  =  IDX-S  +  1
               END-PERFORM
             END-IF
      *--- 2009/11/19 ADD END ------------------------------------------
      *
             IF    WRK-PARA-NYUGAIKBN   =  "1"  OR  SPACE
               IF    S140TMP-NYUGAIKBN   =   "1"
      *          入院フォーム
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S140TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
      *--- 2009/11/19 DELETE START -------------------------------------
      *                    AND    S140TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
      *--- 2009/11/19 DELETE END ---------------------------------------
      *
                   PERFORM  3104-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3205-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  350-PRINT-OUT-SEC
                   MOVE     1     TO   FLG-PASS
                 END-PERFORM
      * 20081014 市町村ごとの集計を追加START
      *          ２枚目（後期高齢者分）
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                        AND ( S140TMP-HKNJANUM(WRK-CNT3)  =  '00000000'
                           OR    S140TMP-HKNJANUM(WRK-CNT3)  =  SPACE )
                   PERFORM  3105-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3206-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  360-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      * 20081014 市町村ごとの集計を追加END
      *
               END-IF
             END-IF
      *
             IF    WRK-PARA-NYUGAIKBN   =  "2"  OR  SPACE
               IF    S140TMP-NYUGAIKBN   =   "2"
      *          外来フォーム
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S140TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
      *--- 2009/11/19 DELETE START -------------------------------------
      *                    AND    S140TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
      *--- 2009/11/19 DELETE END ---------------------------------------
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
      * 20081014 市町村ごとの集計を追加START
      *          ２枚目（後期高齢者分）
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                        AND ( S140TMP-HKNJANUM(WRK-CNT3)  =  '00000000'
                        OR    S140TMP-HKNJANUM(WRK-CNT3)  =  SPACE )
                   PERFORM  3103-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3203-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  340-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      * 20081014 市町村ごとの集計を追加END
      *
               END-IF
             END-IF
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    S140TMP-FILE
      *
           .
       2002-PRINT-TYPE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢帳票編集<ヘッダー部>処理
      *****************************************************************
       3102-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S140G1
      *
      *    ページ数
           ADD     1                  TO   CNT-PAGE
           MOVE    CNT-PAGE           TO   WRK-PAGE
           MOVE    WRK-PAGE           TO   S140G1-PAGE
      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO   IDX4
             MOVE     SUM-SRYKAMAX    TO   IDX-MAX
             PERFORM  UNTIL   IDX4   >   IDX-MAX
               IF  S140TMP-SRYKA   =   SUM-SRYKA(IDX4)
                 MOVE    ZERO         TO   IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO   S140G1-SRYKANAME
               ELSE
                 ADD     1            TO   IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S140TMP-NYUGAIKBN   =   "1"
             MOVE     "入院"          TO   S140G1-NYUGAI
           ELSE
             MOVE     "外来"          TO   S140G1-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM          TO   S140G1-SRYYM
           MOVE    WRK-SEIYMD         TO   S140G1-SYSYMD
      *
           .
       3102-HEAD-HEN-EXT.
           EXIT.
      *
      * 20081014 市町村ごとの集計を追加START
      *****************************************************************
      *    後期高齢帳票編集<ヘッダー部>処理
      *****************************************************************
       3103-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                       S140G2
      *
      *    ページ数
           ADD     1                  TO    CNT-PAGE
           MOVE    CNT-PAGE           TO    WRK-PAGE
           MOVE    WRK-PAGE           TO    S140G2-PAGE
      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF   S140TMP-SRYKA   =   SUM-SRYKA(IDX4)
                 MOVE    ZERO         TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO   S140G2-SRYKANAME
               ELSE
                 ADD     1            TO    IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S140TMP-NYUGAIKBN    =   "1"
             MOVE     "入院"          TO    S140G2-NYUGAI
           ELSE
             MOVE     "外来"          TO    S140G2-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM          TO    S140G2-SRYYM
           MOVE    WRK-SEIYMD         TO    S140G2-SYSYMD
      *
           .
       3103-HEAD-HEN-EXT.
           EXIT.
      *
      * 20081014 市町村ごとの集計を追加END
      *****************************************************************
      *    後期高齢帳票編集<ヘッダー部>処理
      *****************************************************************
       3104-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                 S140N1
      *
      *    ページ数
           ADD     1                  TO   CNT-PAGE
           MOVE    CNT-PAGE           TO   WRK-PAGE
           MOVE    WRK-PAGE           TO   S140N1-PAGE
      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO   IDX4
             MOVE     SUM-SRYKAMAX    TO   IDX-MAX
             PERFORM  UNTIL    IDX4   >   IDX-MAX
               IF  S140TMP-SRYKA   =   SUM-SRYKA(IDX4)
                 MOVE    ZERO         TO   IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)  TO   S140N1-SRYKANAME
               ELSE
                 ADD     1            TO   IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S140TMP-NYUGAIKBN   =    "1"
             MOVE     "入院"          TO   S140N1-NYUGAI
           ELSE
             MOVE     "外来"          TO   S140N1-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM          TO   S140N1-SRYYM
           MOVE    WRK-SEIYMD         TO   S140N1-SYSYMD
      *
           .
       3104-HEAD-HEN-EXT.
           EXIT.
      * 20081014 市町村ごとの集計を追加START
      *****************************************************************
      *    後期高齢帳票編集<ヘッダー部>処理
      *****************************************************************
       3105-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                 S140N2
      *
      *    ページ数
      * 20040322 編集 Y.Takahashi
           ADD     1                  TO   CNT-PAGE
           MOVE    CNT-PAGE           TO   WRK-PAGE
           MOVE    WRK-PAGE           TO   S140N2-PAGE
      *      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO   IDX4
             MOVE     SUM-SRYKAMAX    TO   IDX-MAX
             PERFORM  UNTIL  IDX4  >  IDX-MAX
               IF  S140TMP-SRYKA   =   SUM-SRYKA(IDX4)
                 MOVE    ZERO         TO   IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO   S140N2-SRYKANAME
               ELSE
                 ADD     1            TO   IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S140TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"          TO   S140N2-NYUGAI
           ELSE
             MOVE     "外来"          TO   S140N2-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM          TO   S140N2-SRYYM
           MOVE    WRK-SEIYMD         TO   S140N2-SYSYMD
      *
           MOVE    WRK-CITYNAME       TO   S140N2-CITYKBN
      *
           .
       3105-HEAD-HEN-EXT.
           EXIT.
      *
      * 20081014 市町村ごとの集計を追加END
      *****************************************************************
      *    後期高齢帳票編集<明細>処理
      *****************************************************************
       3202-BODY-HEN-SEC               SECTION.
      *
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >  3
                      OR    FLG-PASS  = 1
      *    後期高齢分
      *    件数
             MOVE     S140TMP-KENSU(IDX1)   TO    WRK-Z4
             MOVE     WRK-Z4                TO    S140G1-KENSU(IDX1)
      *
      *    日数
             MOVE     S140TMP-NISSU(IDX1)   TO    WRK-Z4
             MOVE     WRK-Z4                TO    S140G1-NISSU(IDX1)
      *
      *    点数
             MOVE     S140TMP-TENSU(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G1-TENSU(IDX1)
      *
      *    窓口請求額
      *       MOVE     S140TMP-MADOGK(IDX1)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO    S140G1-MADOGK(IDX1)
             MOVE     S140TMP-MADOGK(IDX1)  TO    WRK-Z93
             MOVE     WRK-Z93               TO    S140G1-MADOGK(IDX1)
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計
             MOVE     S140TMP-JIHIGK(IDX1)  TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G1-JIHIGK(IDX1)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S140TMP-HKNGK(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G1-HKNGK(IDX1)
      *
      *    公費請求額
             MOVE     S140TMP-KOHGK(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G1-KOHGK(IDX1)
      *
      *    医療収益
             MOVE     S140TMP-SYUEKI(IDX1)  TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G1-SYUEKI(IDX1)
      *
           END-PERFORM
      *
      *    合計
           IF    FLG-PASS  =  ZERO
      *
      *    件数
             MOVE     S140TMP-GKENSU         TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S140G1-GKENSU
      *
      *    日数
             MOVE     S140TMP-GNISSU         TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S140G1-GNISSU
      *
      *    点数
             MOVE     S140TMP-GTENSU         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140G1-GTENSU
      *
      *    窓口請求額
      *       MOVE     S140TMP-GMADOGK         TO    WRK-Z92
      *       MOVE     WRK-Z92                TO    S140G1-GMADOGK
             MOVE     S140TMP-GMADOGK        TO    WRK-Z93
             MOVE     WRK-Z93                TO    S140G1-GMADOGK
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計
             MOVE     S140TMP-GJIHIGK        TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140G1-GJIHIGK
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S140TMP-GHKNGK         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140G1-GHKNGK
      *
      *    公費請求額
             MOVE     S140TMP-GKOHGK         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140G1-GKOHGK
      *
      *    医療収益
             MOVE     S140TMP-GSYUEKI        TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140G1-GSYUEKI
           END-IF
      *
      *    後期高齢・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3   >  20
                    OR    S140TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
      * 20081014 市町村ごとの集計を追加START
      *              OR       WRK-CNT1  >  100
                    OR    WRK-CNT1  >  300
      * 20081014 市町村ごとの集計を追加END
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S140TMP-KOH1-KEY(WRK-CNT1)   TO    WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G          TO    S140G1-KOHNAME(IDX3)
      *
      *    件数
             MOVE     S140TMP-KKENSU(WRK-CNT1)     TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S140G1-KKENSU(IDX3)
      *
      *    日数
             MOVE     S140TMP-KNISSU(WRK-CNT1)     TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S140G1-KNISSU(IDX3)
      *
      *    点数
             MOVE     S140TMP-KTENSU(WRK-CNT1)     TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140G1-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT1
           END-PERFORM
      *
           .
       3202-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢帳票編集<明細>処理(入院：１枚目)
      *****************************************************************
       3205-BODY-HEN-SEC               SECTION.
      *
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >   3
                      OR    FLG-PASS  =  1
      *    後期高齢分
      *    件数
             MOVE     S140TMP-KENSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S140N1-KENSU(IDX1)
      *
      *    日数
             MOVE     S140TMP-NISSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S140N1-NISSU(IDX1)
      *
      *    点数
             MOVE     S140TMP-TENSU(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140N1-TENSU(IDX1)
      *
      *    窓口請求額
      *       MOVE     S140TMP-MADOGK(IDX1)   TO    WRK-Z92
      *       MOVE     WRK-Z92                TO    S140N1-MADOGK(IDX1)
             MOVE     S140TMP-MADOGK(IDX1)   TO    WRK-Z93
             MOVE     WRK-Z93                TO    S140N1-MADOGK(IDX1)
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計
             MOVE     S140TMP-JIHIGK(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140N1-JIHIGK(IDX1)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S140TMP-HKNGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140N1-HKNGK(IDX1)
      *
      *    公費請求額
             MOVE     S140TMP-KOHGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140N1-KOHGK(IDX1)
      *
      *    医療収益
             MOVE     S140TMP-SYUEKI(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140N1-SYUEKI(IDX1)
      ***  食事療養
      *    日数
      *--- 2009/08/20 CHANGE START -------------------------------------
      *       MOVE     S140TMP-SNISSU(IDX1)   TO    WRK-Z4
      *       MOVE     WRK-Z4                 TO    S140N1-SNISSU(IDX1)
             MOVE     S140TMP-SNISSU(IDX1)   TO    WRK-Z52
             MOVE     WRK-Z52                 TO    S140N1-SNISSU(IDX1)
      *--- 2009/08/20 CHANGE END ---------------------------------------
      *
      *    請求額
             MOVE     S140TMP-SSEIKYU(IDX1)  TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140N1-SSEIKYU(IDX1)
      *
      *    標準負担額
             MOVE     S140TMP-SFTNGK(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92                TO    S140N1-SFTNGK(IDX1)
           END-PERFORM
      *
      *
      *    後期高齢・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >  20
                    OR    S140TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
      * 20081014 市町村ごとの集計を追加START
      *              OR       WRK-CNT1  >  100
                    OR    WRK-CNT1  >  300
      * 20081014 市町村ごとの集計を追加END
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S140TMP-KOH1-KEY(WRK-CNT1)    TO    WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G      TO    S140N1-KOHNAME(IDX3)
      *    件数
             MOVE     S140TMP-KKENSU(WRK-CNT1)      TO    WRK-Z4
             MOVE     WRK-Z4             TO    S140N1-KKENSU(IDX3)
      *
      *    日数
             MOVE     S140TMP-KNISSU(WRK-CNT1)      TO    WRK-Z4
             MOVE     WRK-Z4             TO    S140N1-KNISSU(IDX3)
      *
      *    点数
             MOVE     S140TMP-KTENSU(WRK-CNT1)      TO    WRK-Z92
             MOVE     WRK-Z92            TO    S140N1-KTENSU(IDX3)
             ADD      1                  TO    WRK-CNT1
           END-PERFORM
      *
      *    合計
           IF    FLG-PASS  =  ZERO
      *
      *    件数
             MOVE     S140TMP-GKENSU        TO    WRK-Z4
             MOVE     WRK-Z4                TO    S140N1-GKENSU
      *
      *    日数
             MOVE     S140TMP-GNISSU        TO    WRK-Z4
             MOVE     WRK-Z4                TO    S140N1-GNISSU
      *
      *    点数
             MOVE     S140TMP-GTENSU        TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N1-GTENSU
      *
      *    窓口請求額
      *       MOVE     S140TMP-GMADOGK       TO    WRK-Z92
      *       MOVE     WRK-Z92               TO    S140N1-GMADOGK
             MOVE     S140TMP-GMADOGK       TO    WRK-Z93
             MOVE     WRK-Z93               TO    S140N1-GMADOGK
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計
             MOVE     S140TMP-GJIHIGK       TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N1-GJIHIGK
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S140TMP-GHKNGK        TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N1-GHKNGK
      *
      *    公費請求額
             MOVE     S140TMP-GKOHGK        TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N1-GKOHGK
      *
      *    医療収益
             MOVE     S140TMP-GSYUEKI       TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N1-GSYUEKI
      ***  食事療養
      *    日数
      *--- 2009/08/20 CHANGE START -------------------------------------
      *       MOVE     S140TMP-SGNISSU       TO    WRK-Z4
      *       MOVE     WRK-Z4                TO    S140N1-SGNISSU
             MOVE     S140TMP-SGNISSU       TO    WRK-Z52
             MOVE     WRK-Z52               TO    S140N1-SGNISSU
      *--- 2009/08/20 CHANGE END ---------------------------------------
      *
      *    請求額
             MOVE     S140TMP-SGSEIKYU      TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N1-SGSEIKYU
      *
      *    標準負担額
             MOVE     S140TMP-SGFTNGK       TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N1-SGFTNGK
           END-IF
      *
           .
       3205-BODY-HEN-EXT.
           EXIT.
      *
      * 20081014 市町村ごとの集計を追加START
      *****************************************************************
      *    後期高齢帳票編集<明細>処理(外来：２枚目)
      *****************************************************************
       3203-BODY-HEN-SEC               SECTION.
      *
      *    後期高齢・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR   S140TMP-HKNJANUM(WRK-CNT3)  =  SPACE
                    OR   WRK-CNT3   >   300
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S140TMP-HKNJANUM(WRK-CNT3)  TO  WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME     TO    S140G2-HKNJANAME(IDX3)
      *    件数
             MOVE     S140TMP-HKENSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S140G2-KENSU(IDX3)
      *
      *    日数
             MOVE     S140TMP-HNISSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S140G2-NISSU(IDX3)
      *
      *    点数
             MOVE     S140TMP-HTENSU(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G2-TENSU(IDX3)
      *
      *    窓口請求額
      *       MOVE     S140TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO    S140G2-MADOGK(IDX3)
             MOVE     S140TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S140G2-MADOGK(IDX3)
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計
             MOVE     S140TMP-HJIHIGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G2-JIHIGK(IDX3)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S140TMP-HHKNGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S140TMP-HKOHGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S140TMP-HSYUEKI(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140G2-SYUEKI(IDX3)
             ADD      1                     TO    WRK-CNT3
           END-PERFORM
           .
       3203-BODY-HEN-EXT.
           EXIT.
      *
      * 20081014 市町村ごとの集計を追加END
      *****************************************************************
      *    後期高齢帳票編集<明細>処理(入院：２枚目)
      *****************************************************************
       3206-BODY-HEN-SEC               SECTION.
      *
      *
      *    後期高齢・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >  55
                    OR   S140TMP-HKNJANUM(WRK-CNT3)  =  SPACE
                    OR   WRK-CNT3  >  300
      *
      *    保険者名
             INITIALIZE           WRK-HKNJANUM
             MOVE     S140TMP-HKNJANUM(WRK-CNT3)  TO    WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME         TO    S140N2-HKNJANAME(IDX3)
      *    件数
             MOVE     S140TMP-HKENSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S140N2-KENSU(IDX3)
      *
      *    日数
             MOVE     S140TMP-HNISSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S140N2-NISSU(IDX3)
      *
      *    点数
             MOVE     S140TMP-HTENSU(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N2-TENSU(IDX3)
      *
      *    窓口請求額
      *       MOVE     S140TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z92
      *       MOVE     WRK-Z92               TO    S140N2-MADOGK(IDX3)
             MOVE     S140TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S140N2-MADOGK(IDX3)
      *
      *--- (2008/12/12) line add start monma ---------------------------
      *    自費計
             MOVE     S140TMP-HJIHIGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N2-JIHIGK(IDX3)
      *--- (2008/12/12) line add end   monma ---------------------------
      *
      *    保険請求額
             MOVE     S140TMP-HHKNGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S140TMP-HKOHGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S140TMP-HSYUEKI(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N2-SYUEKI(IDX3)
      ***  食事療養
      *    日数
      *--- 2009/08/20 CHANGE START -------------------------------------
      *       MOVE     S140TMP-HSNISSU(WRK-CNT3)   TO    WRK-Z4
      *       MOVE     WRK-Z4                TO    S140N2-SNISSU(IDX3)
             MOVE     S140TMP-HSNISSU(WRK-CNT3)   TO    WRK-Z52
             MOVE     WRK-Z52                TO    S140N2-SNISSU(IDX3)
      *--- 2009/08/20 CHANGE END ---------------------------------------
      *
      *    請求額
             MOVE     S140TMP-HSSEIKYU(WRK-CNT3)  TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N2-SSEIKYU(IDX3)
      *
      *    標準負担額
             MOVE     S140TMP-HSFTNGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S140N2-SFTNGK(IDX3)
             ADD      1                     TO    WRK-CNT3
           END-PERFORM
      *
      *
           .
       3206-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
      *
           INITIALIZE                        ORCSPRTAREA
           MOVE    "INS"                       TO   SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                               TO   SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      *--- 2008/10/23  CHANGE  START -----------------------------------
      *     MOVE    "A00000S140G.red"         TO  SPRT-PRTID
           MOVE    "A00000S140G1.red"          TO   SPRT-PRTID
      *--- 2008/10/23  CHANGE  END -----------------------------------
           MOVE    S100LNK-SPRT-TITLE          TO   SPRT-TITLE
           MOVE    S140G1                      TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
           CALL    "ORCSPRT"                   USING
                                               ORCSPRTAREA
                                               SPA-AREA
           IF   SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"   TO   WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
	       END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      * 20081014 市町村ごとの集計を追加START
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                      TO   SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM    TO   SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY   TO   SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP TO   SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM     TO   SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD    TO   SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID   TO   SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY  TO   SPRT-PRIORITY
           MOVE    "A00000S140G2.red"         TO   SPRT-PRTID
           MOVE    S100LNK-SPRT-TITLE         TO   SPRT-TITLE
           MOVE    S140G2                     TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID    TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID      TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM     TO   SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF  SPRT-RETURN   =   ZERO
             CONTINUE               ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
	         PERFORM   500-ERR-HENSYU-SEC
	       END-IF
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       360-PRINT-OUT-SEC         SECTION.
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
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
           MOVE   "A00000S140N2.red"           TO   SPRT-PRTID
           MOVE    S100LNK-SPRT-TITLE          TO   SPRT-TITLE
           MOVE    S140N2                      TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF  SPRT-RETURN   =   ZERO
             CONTINUE               ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
	         PERFORM   500-ERR-HENSYU-SEC
	       END-IF
           .
       360-PRINT-OUT-EXT.
           EXIT.
      *
      * 20081014 市町村ごとの集計を追加END
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       350-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                     TO   SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM   TO   SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                             TO   SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                             TO   SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM    TO   SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD   TO   SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID
                                             TO   SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY TO   SPRT-PRIORITY
      *--- 2008/10/23  CHANGE  START -----------------------------------
      *     MOVE   "A00000S140N.red"          TO  SPRT-PRTID
           MOVE   "A00000S140N1.red"         TO   SPRT-PRTID
      *--- 2008/10/23  CHANGE  END -----------------------------------
           MOVE    S100LNK-SPRT-TITLE        TO   SPRT-TITLE
           MOVE    S140N1                    TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID   TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID     TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM    TO   SPRT-PRTNM
           CALL    "ORCSPRT"                 USING
                                             ORCSPRTAREA
                                             SPA-AREA
           IF  SPRT-RETURN         =   ZERO
               CONTINUE                ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                     TO  WRK-RECEERR
	           PERFORM 500-ERR-HENSYU-SEC
	       END-IF
           .
       350-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費名編集処理
      *****************************************************************
       333-KOHNAME-HEN-SEC               SECTION.
      *
           MOVE    SPACE                 TO    WRK-KOHNAME-G
                                               WRK-KOHNAME-G2
           MOVE    WRK-KOHTMP-KOHNUM     TO    WRK-KOHNUM
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
           MOVE   WRK-KOHNAME-G          TO    WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE   WRK-OUT-INPUT          TO    WRK-KOHNAME-G
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
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_syskanri"        TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC     =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_syskanri"      TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    ZERO              TO   FLG-SYSKANRI
             ELSE
               MOVE    1                 TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                   TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_syskanri"        TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
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
           MOVE    SYS-1005-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-SYSKANRI3
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
           MOVE    "DBFETCH"         TO   MCP-FUNC
           MOVE    "tbl_syskanri"    TO   MCP-TABLE
           MOVE    "key2"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    ZERO            TO   FLG-SYSKANRI3
             MOVE    MCPDATA-REC     TO   SYS-1005-REC
           ELSE
             MOVE    1               TO   FLG-SYSKANRI3
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
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           INITIALIZE                STS-AREA-DAY
           INITIALIZE                LNK-DAY7-AREA
           MOVE    "71"              TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD          TO   LNK-DAY7-YM
           CALL    "ORCSDAY"         USING
                                     STS-AREA-DAY
                                     LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI   TO   WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                    WRK-OUT-INPUT
      *
           INITIALIZE                    ORCSKANACHKAREA
           MOVE    "2"                   TO   KANACHK-SYORI
           MOVE    WRK-MAE-INPUT         TO   KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"         USING
                                         ORCSKANACHKAREA
           IF   KANACHK-RC   =   ZERO
             MOVE    KANACHK-OUT-INPUT   TO   WRK-OUT-INPUT
           ELSE
             MOVE    WRK-MAE-INPUT       TO   WRK-OUT-INPUT
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
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT            RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *      ジョブ終了処理
             MOVE    "JBE"             TO   SJOBKANRI-MODE
             INITIALIZE                  JOBKANRI-REC
             MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
             MOVE    S100LNK-JOBID     TO   JOB-JOBID
             MOVE    S100LNK-SHELLID   TO   JOB-SHELLID
             MOVE    WRK-RECEERR       TO   JOB-YOBI
             MOVE    "9999"            TO   JOB-ERRCD
             CALL    "ORCSJOB"         USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1        TO   FLG-END
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
           READ    S140TMP-FILE
             INVALID    KEY
               MOVE    1           TO   FLG-S140TMP
             NOT INVALID  KEY
               MOVE    ZERO        TO   FLG-S140TMP
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
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_syskanri"        TO   MCP-TABLE
           MOVE    "key10"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_syskanri"      TO   MCP-TABLE
             MOVE    "key10"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF      MCP-RC      =   ZERO
               MOVE    ZERO              TO   FLG-SYSKANRI
             ELSE
               MOVE    1                 TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                   TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_syskanri"        TO   MCP-TABLE
           MOVE    "key10"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
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
           READ  S140TMP-FILE   NEXT
             AT  END
               MOVE     1       TO   FLG-PRINT
             NOT AT END
               ADD      1       TO   CNT-READ
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
           INITIALIZE                   RECE10-REC
      *
           MOVE   WRK-PARA-HOSPNUM      TO   RECE10-HOSPNUM.
           MOVE   WRK-PARA-SRYYM        TO   RECE10-SKYYM.
      * 20080617 処理速度の向上の修正START tanaka
           MOVE    "6"                  TO   RECE10-TEISYUTUSAKI
      * 20080617 処理速度の向上の修正END
      *
           MOVE    RECE10-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_seikyu"         TO   MCP-TABLE
      * 20080617 処理速度の向上の修正START tanaka
      *     MOVE    "all"                TO  MCP-PATHNAME
           MOVE    "key60"              TO   MCP-PATHNAME
      * 20080617 処理速度の向上の修正END
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 900-SEIKYU-READ-SEC
           ELSE
             MOVE    1                  TO   FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"              TO   MCP-FUNC
           MOVE    "tbl_seikyu"           TO   MCP-TABLE
      * 20080617 処理速度の向上の修正START tanaka
      *     MOVE    "all"                TO  MCP-PATHNAME
           MOVE    "key60"                TO   MCP-PATHNAME
      * 20080617 処理速度の向上の修正END
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC        =   ZERO
      *
             MOVE    ZERO                 TO   FLG-END
             MOVE    MCPDATA-REC          TO   RECE10-REC

      *    テスト患者のときは読み飛ばす
             MOVE    SPACE                TO   PTINF-REC
             INITIALIZE                        PTINF-REC
             MOVE    RECE10-HOSPNUM       TO   WRK-PARA-HOSPNUM
             MOVE    RECE10-PTID          TO   WRK-PTID
             PERFORM   900-PTINF-READ-SEC
             IF   FLG-PTINF           =   ZERO
                 AND   PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-SEIKYU-READ-SEC
             END-IF
             PERFORM  VARYING  IDX5  FROM  1  BY  1
                      UNTIL    IDX5  >     4  OR
                      RECE10-KOHNUM(IDX5)  =  SPACE
               IF    RECE10-KOHNUM(IDX5)  =  "091"
                 MOVE    "051"            TO   RECE10-KOHNUM(IDX5)
               END-IF
             END-PERFORM
           ELSE
             MOVE    1                    TO   FLG-END
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
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_seikyu"        TO   MCP-TABLE
      * 20080617 処理速度の向上の修正START tanaka
      *     MOVE    "all"                TO  MCP-PATHNAME
           MOVE    "key60"             TO   MCP-PATHNAME
      * 20080617 処理速度の向上の修正END
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                   PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO   PTINF-HOSPNUM
           MOVE    WRK-PTID            TO   PTINF-PTID
           MOVE    PTINF-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_ptinf"       TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
      *--- 2009/11/19 ADD START ----------------------------------------
		   MOVE    SYU-SYUHKNNUM       TO  PTHKN-HKNNUM
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKSTYMD
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKEDYMD
      *--- 2009/11/19 ADD END ------------------------------------------
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
      *--- 2009/11/19 CHANGE START -------------------------------------
      *     MOVE    "key6"              TO  MCP-PATHNAME
           MOVE    "key9"              TO  MCP-PATHNAME
      *--- 2009/11/19 CHANGE END ---------------------------------------
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
      *--- 2009/11/19 CHANGE START -------------------------------------
      *     MOVE    "key6"              TO  MCP-PATHNAME
           MOVE    "key9"              TO  MCP-PATHNAME
      *--- 2009/11/19 CHANGE END ---------------------------------------
             CALL    "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF      MCP-RC            =   ZERO
               MOVE    MCPDATA-REC   TO  PTHKNINF-REC
               MOVE    ZERO          TO  FLG-PTHKNINF
             ELSE
               MOVE    1             TO  FLG-PTHKNINF
               INITIALIZE            PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1               TO  FLG-PTHKNINF
             INITIALIZE              PTHKNINF-REC
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"         TO  MCP-TABLE
      *--- 2009/11/19 CHANGE START -------------------------------------
      *     MOVE    "key6"              TO  MCP-PATHNAME
           MOVE    "key9"              TO  MCP-PATHNAME
      *--- 2009/11/19 CHANGE END ---------------------------------------
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *--- 2009/06/26 ADD END   ----------------------------------------
      *--- 2009/07/29 ADD START ----------------------------------------
      *****************************************************************
      *    患者保険情報マスター読込処理
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    SYU-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SYU-PTID            TO  PTHKN-PTID
           MOVE    SYU-SYUHKNNUM       TO  PTHKN-HKNNUM
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKSTYMD
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key9"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF      MCP-RC    =   ZERO
               MOVE    MCPDATA-REC   TO  PTHKNINF-REC
               MOVE    ZERO          TO  FLG-PTHKNINF
             ELSE
               MOVE    1             TO  FLG-PTHKNINF
               INITIALIZE            PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1               TO  FLG-PTHKNINF
             INITIALIZE              PTHKNINF-REC
           END-IF
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
      *--- 2009/07/29 ADD END   ----------------------------------------
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE    WRK-PARA-HOSPNUM   TO   HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM       TO   HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_hknjainf"     TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE  "HKNJAINF-KEY"   TO   ORC-DBPATH
               PERFORM  925-HKNJAINF-READ-SEC
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE  "tbl_hknjainf"       TO   MCP-TABLE
           MOVE  "key"                TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
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
           MOVE  "DBFETCH"               TO   MCP-FUNC
           MOVE  "tbl_hknjainf"          TO   MCP-TABLE
           MOVE  "key"                   TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC           TO   HKNJAINF-REC
             MOVE  ZERO                  TO   FLG-HKNJAINF
             MOVE  HKNJA-HKNJANAME       TO   WRK-HKNJANAME
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1                     TO   FLG-HKNJAINF
             MOVE  WRK-HKNJANUM          TO   WRK-HKNJANAME
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
      *--- 2009/07/29 ADD START ----------------------------------------
           IF   RECE10-FTNMONEY(1)  =  ZERO
               AND   RECE10-KYURATE  NOT =  ZERO
             PERFORM  VARYING  IDX7  FROM  1  BY  1
                      UNTIL  IDX7  >  4
                      OR  SYU-KOH-HKNNUM(IDX7)  =  SPACE
               IF   FLG-TYOKI  =  1
                   AND  SYU-KOH-HKNNUM(IDX7)  =  "972"
                 COMPUTE  WRK-HKNSKYMONEY  =  (SYU-TOTAL-HKNTEN  *  10)
                                         -  SYU-KOH-COMPFTN-ENTANI(IDX7)
                 ADD    WRK-HKNSKYMONEY   TO   WRK-HKNGK
                 MOVE    SYU-KOH-COMPFTN-ENTANI(IDX7)
                                              TO   SYU-SYUCOMPFTN-ENTANI
                 MOVE    SYU-KOH-COMPFTN(IDX7)  TO   SYU-SYUCOMPFTN
      *--- 2010/01/08 ADD START ----------------------------------------
      *           MOVE    SYU-KOH-COMPFTN-ENTANI(IDX7)  TO  WRK-TYOKIFTN
      *--- 2010/01/08 ADD END ------------------------------------------
               END-IF
               IF   FLG-TYOKI  =  2
                   AND  SYU-KOH-HKNNUM(IDX7)  =  "974"
                 COMPUTE  WRK-HKNSKYMONEY  =  (SYU-TOTAL-HKNTEN  *  10)
                                         -  SYU-KOH-COMPFTN-ENTANI(IDX7)
                 ADD    WRK-HKNSKYMONEY   TO   WRK-HKNGK
                 MOVE    SYU-KOH-COMPFTN-ENTANI(IDX7)
                                              TO   SYU-SYUCOMPFTN-ENTANI
                 MOVE    SYU-KOH-COMPFTN(IDX7)  TO   SYU-SYUCOMPFTN
      *--- 2010/01/08 ADD START ----------------------------------------
      *           MOVE    SYU-KOH-COMPFTN-ENTANI(IDX7)  TO  WRK-TYOKIFTN
      *--- 2010/01/08 ADD END ------------------------------------------
               END-IF
             END-PERFORM
      *--- 2010/04/06 ADD START ----------------------------------------
             IF   ( FLG-TYOKI  =  1  OR  2 )
                 AND  ( SYU-KOH-HKNNUM(1)  NOT =  "972" AND "974" )
                 AND  ( SYU-KOH-HKNNUM(2)  NOT =  "972" AND "974" )
                 AND  ( SYU-KOH-HKNNUM(3)  NOT =  "972" AND "974" )
                 AND  ( SYU-KOH-HKNNUM(4)  NOT =  "972" AND "974" )
                COMPUTE  WRK-HKNGK2  =  SYU-TOTAL-HKNTEN
                                     *  RECE10-KYURATE
                ADD    WRK-HKNGK2    TO   WRK-HKNGK
             END-IF
      *--- 2010/04/06 ADD END ------------------------------------------
           END-IF
      *--- 2009/07/29 ADD END ------------------------------------------
      *
           IF  RECE10-RJNHKNNUM NOT = SPACE
             MOVE   SYU-KOH-COMPFTN(1) TO  WRK-ZENCOMPFTN
           ELSE
             MOVE   SYU-SYUCOMPFTN     TO  WRK-ZENCOMPFTN
           END-IF
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                UNTIL    IDX  >    4 OR
                         SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF    SYU-KOH-HKNNUM(IDX) NOT  =  "027"
      *--- 2009/08/18 ADD START ----------------------------------------
                 AND  SYU-KOH-HKNNUM(IDX)  NOT =  "972"
                 AND  SYU-KOH-HKNNUM(IDX)  NOT =  "974"
      *--- 2009/08/18 ADD END ------------------------------------------
      *    差額から助成額(公費請求額)を算出
               IF    RECE10-RJNHKNNUM  NOT = SPACE
      *
                 IF  SYU-KOH-COMPFTN(IDX) < WRK-ZENCOMPFTN
      *
                   COMPUTE  WRK-KOHSEIKYUGK  =  SYU-KOH-COMPFTN(1)
                                             -  SYU-KOH-COMPFTN(IDX)
                   COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SKJ-FTN(1)
                                                -  SYU-SKJ-FTN(IDX)
                   MOVE   SYU-KOH-COMPFTN(IDX)  TO  WRK-ZENCOMPFTN
                 END-IF
               ELSE
      *
      *--- 2009/08/18 DELETE START -------------------------------------
      *           IF  SYU-KOH-COMPFTN(IDX)  <  WRK-ZENCOMPFTN
      *--- 2009/08/18 DELETE END ---------------------------------------
      *
      *--- 2009/01/08 ADD+DELETE START ---------------------------------
      *             COMPUTE  WRK-KOHSEIKYUGK  =  SYU-SYUCOMPFTN
      *                                       -  SYU-KOH-COMPFTN(IDX)
      *             COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
      *                                          -  SYU-SKJ-FTN(IDX)
                 IF    RECE10-FTNMONEY(1)  =  ZERO
                     AND  RECE10-KYURATE  NOT = ZERO
      *    限度なし
                   MOVE  SYU-KOH-HKNNUM(IDX)   TO  WRK-KOHNUM
                   PERFORM   950-HKNNUM-SET-SEC
      *--- 2009/07/01 DELETE START -------------------------------------
      *               IF   HKN-RECEFTNDSPKBN  =  1
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
                 MOVE   SYU-KOH-COMPFTN(IDX)  TO  WRK-ZENCOMPFTN
      *--- 2009/08/18 DELETE START -------------------------------------
      *           END-IF
      *--- 2009/08/18 DELETE END ---------------------------------------
               END-IF
      *--- 2010/08/04 ADD START ---------------------------------------- 
               ELSE
                 IF   RECE10-FLG(2)  =  1
                     AND  ( FLG-TYOKI  =  1  OR  2 )
                     AND  ( SYU-KOH-HKNNUM(2)  =  SPACE
                     AND  SYU-KOH-HKNNUM(3)  =  SPACE
                     AND  SYU-KOH-HKNNUM(4)  =  SPACE )

                   COMPUTE  WRK-HKNGK  =  WRK-HKNGK
                                       +  SYU-SYUCOMPFTN-ENTANI
                                       -  SYU-SKYMONEY
                 END-IF
      *--- 2010/08/04 ADD END ------------------------------------------ 
             END-IF
           END-PERFORM
      *
      *--- 2009/01/08 ADD START ----------------------------------------
           IF  WRK-FTNFLG  =  1
             COMPUTE  WRK-HKNFTN  =  WRK-HKNFTN  -  WRK-KOHFTN
      *--- 2009/08/18 ADD START ----------------------------------------
             IF  WRK-HKNFTN  <=  10
               MOVE    ZERO      TO   WRK-HKNFTN
             END-IF
      *--- 2009/08/18 ADD END ------------------------------------------
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
      *     ADD    WRK-KOHSEIKYUGK       TO   WRK-KOHSEIKYUGK-G
      *--- 2009/01/08 DELETE END ---------------------------------------
           ADD    WRK-KOHSKJSEIKYUGK    TO   WRK-KOHSEIKYUGK-G
           ADD    SYU-SKYMONEY          TO   WRK-MADOSEIKYUGK-G
      *--- 2009/07/29 ADD START ----------------------------------------
      *--- 2009/08/18 DELETE START -------------------------------------
      *     COMPUTE   WRK-MSKYMONEY   =  WRK-MADOSEIKYUGK-G
      *                               -  RECE10-SHOKUJIFTN(1)
      *--- 2009/08/18 DELETE END ---------------------------------------
      *--- 2010/01/08 ADD START ----------------------------------------
      *     IF   FLG-TYOKI  =  1
      *       IF   WRK-TYOKIFTN  =  ZERO
      *         MOVE    ZERO      TO   WRK-KOHSEIKYUGK-G
      **--- 2009/08/18 DELETE START -------------------------------------
      **       ELSE
      **         COMPUTE  WRK-KOHSEIKYUGK-G  =  10000 - WRK-MSKYMONEY
      **--- 2009/08/18 DELETE END ---------------------------------------
      *       END-IF
      *     END-IF
      *     IF   FLG-TYOKI  =  2
      *       IF   WRK-TYOKIFTN  =  ZERO
      *         MOVE    ZERO      TO   WRK-KOHSEIKYUGK-G
      **--- 2009/08/18 DELETE START -------------------------------------
      **       ELSE
      **         COMPUTE  WRK-KOHSEIKYUGK-G  =  20000 - WRK-MSKYMONEY
      **--- 2009/08/18 DELETE END ---------------------------------------
      *       END-IF
      *     END-IF
      *--- 2009/07/29 ADD END ------------------------------------------
      *--- 2010/01/08 ADD END ------------------------------------------
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
           INITIALIZE     SYUNOU-REC
      *--- 2009/01/08 ADD START ----------------------------------------
                          WRK-HKNFTN
           MOVE   RECE10-FTNMONEY(1)    TO   WRK-HKNFTN
      *--- 2009/01/08 ADD END ------------------------------------------
           MOVE    RECE10-HOSPNUM       TO   SYU-HOSPNUM
           MOVE    RECE10-NYUGAIKBN     TO   SYU-NYUGAIKBN
           MOVE    RECE10-PTID          TO   SYU-PTID
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "SYUNOU-KEY2"      TO   ORC-DBPATH
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
      *--- 2009/07/29 ADD START ----------------------------------------
                 PERFORM  920-PTHKNINF-READ-SEC
      *--- 2009/08/18 ADD START ----------------------------------------
                 IF    RECE10-KOHNUM(1)  NOT =   SPACE
                     AND RECE10-HKNNUM   =   SPACE
                   MOVE    ZERO     TO   PTHKN-HKNJANUM
                 END-IF
      *--- 2009/08/18 ADD END ------------------------------------------
      *--- 2009/07/29 ADD END ------------------------------------------
      *--- 2009/06/26 ADD START ----------------------------------------
               IF  RECE10-SRYYM  =  SYU-SRYYMD(1:6)
                   AND  SYU-DENPJTIKBN   NOT = "3"
      *--- 2009/11/06 DELETE START -------------------------------------
      *             AND  RECE10-TEKSTYMD  <=  SYU-SRYYMD
      *--- 2009/11/06 DELETE END ---------------------------------------
      *
                   AND  SYU-SYUHKNNUM  =  RECE10-HKNNUM
      *--- 2009/07/29 ADD START ----------------------------------------
                   AND  PTHKN-HKNJANUM  =  RECE10-HKNJANUM
      *--- 2009/07/29 ADD END ------------------------------------------
      *
      *    旧総合病院の時は該当する診療科のみ対象にする
      *--- 2008/10/30  CHANGE START ------------------------------------
      * 20081014 主科設定時に診療科を印字するSTART
                 IF   SYS-1001-HOSPSBT1  NOT =  1
                     OR   RECE10-NYUGAIKBN   =   "1"
      *            IF    RECE10-NYUGAIKBN   =   "1" OR
      * 20081014 主科設定時に診療科を印字するEND
      *--- 2008/10/30  CHANGE END --------------------------------------
                     OR   RECE10-RECEKA  =  SYU-SRYKA
                   PERFORM  800-SEIKYU-SEC
                 END-IF
               END-IF
      *--- 2009/06/26 ADD START ----------------------------------------
               END-IF
      *--- 2009/06/26 ADD START ----------------------------------------
      *
               MOVE  "SYUNOU-KEY2"       TO   ORC-DBPATH
               PERFORM   925-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO   MCP-FUNC
           MOVE  "tbl_syunou"            TO   MCP-TABLE
           MOVE  "key2"                  TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
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
           MOVE  "DBFETCH"         TO   MCP-FUNC
           MOVE  "tbl_syunou"      TO   MCP-TABLE
           MOVE  "key2"            TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"       USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC     TO   SYUNOU-REC
             MOVE  ZERO            TO   FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1               TO   FLG-SYUNOU
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
           INITIALIZE                 SYUNOU-REC
           MOVE  JYURRK-HOSPNUM       TO   SYU-HOSPNUM
           MOVE  JYURRK-NYUGAIKBN     TO   SYU-NYUGAIKBN
           MOVE  JYURRK-PTID          TO   SYU-PTID
           MOVE  JYURRK-DENPNUM       TO   SYU-DENPNUM
           MOVE  SYUNOU-REC           TO   MCPDATA-REC
           MOVE  "DBSELECT"           TO   MCP-FUNC
           MOVE  "tbl_syunou"         TO   MCP-TABLE
           MOVE  "key"                TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"          TO   MCP-FUNC
             MOVE  "tbl_syunou"       TO   MCP-TABLE
             MOVE  "key"              TO   MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF  MCP-RC  =  ZERO
                 MOVE  MCPDATA-REC    TO   SYUNOU-REC
                 MOVE  ZERO           TO   FLG-SYUNOU
             ELSE
                 INITIALIZE  SYUNOU-REC
                 MOVE  1              TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                TO   FLG-SYUNOU
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE  "tbl_syunou"         TO   MCP-TABLE
           MOVE  "key"                TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       940-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納情報SELECT(入院データ用)
      *****************************************************************
       940-SYUNOU-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SYUNOU-REC
           MOVE    JYURRK-HOSPNUM       TO   SYU-HOSPNUM
           MOVE    JYURRK-PTID          TO   SYU-PTID
           MOVE    JYURRK-NYUGAIKBN     TO   SYU-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD     TO   SYU-SKYSTYMD
           MOVE    WRK-PARA-PRTLYMD     TO   SYU-SKYEDYMD
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 940-SYUNOU-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-SYUNOU
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
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC        TO   SYUNOU-REC
             MOVE    ZERO               TO   FLG-SYUNOU
             IF    SYU-DENPJTIKBN   =   "3"
               GO    TO    940-SYUNOU-FETCH-SEC
             END-IF
      *
             IF   SYU-SYUHKNNUM(1:2)  NOT =   "97" AND "98"
               GO    TO    940-SYUNOU-FETCH-SEC
             END-IF
      *
           ELSE
              MOVE    1                 TO   FLG-SYUNOU
           END-IF
           .
       940-SYUNOU-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    収納情報クローズ
      *****************************************************************
       940-SYUNOU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key5"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       940-SYUNOU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ情報セット
      *****************************************************************
       930-HKNCOMBI-SET-SEC  SECTION.
      *
           INITIALIZE  HKNCOMBI-REC
           MOVE    JYURRK-HOSPNUM       TO   COMB-HOSPNUM
           MOVE    JYURRK-PTID          TO   COMB-PTID
           MOVE    JYURRK-HKNCOMBINUM   TO   COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"            TO   MCP-FUNC
             MOVE  "tbl_hkncombi"       TO   MCP-TABLE
             MOVE  "key"                TO   MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC        TO   HKNCOMBI-REC
               MOVE  ZERO               TO   FLG-HKNCOMBI
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE  1                  TO   FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                  TO   FLG-HKNCOMBI
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO   MCP-FUNC
           MOVE  "tbl_hkncombi"         TO   MCP-TABLE
           MOVE  "key"                  TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       930-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD START ----------------------------------------
      ******************************************************************
      *    受診履歴情報検索
      ******************************************************************
       920-DBSELECT3-SEC                 SECTION.
      *
           INITIALIZE                 JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM   TO   JYURRK-HOSPNUM
           MOVE    WRK-NYUGAIKBN      TO   JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD   TO   JYURRK-SRYYMD
           MOVE    WRK-PARA-PRTLYMD   TO   JYURRK-UPSRYYMD
           MOVE    JYURRK-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key21"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC    =   ZERO
             PERFORM 920-DBFETCH3-SEC
           ELSE
             MOVE    1                TO   FLG-JYURRK
           END-IF
           .
       920-DBSELECT3-EXT.
           EXIT.
      *
      ******************************************************************
      *     受診履歴情報フェッチ
      ******************************************************************
       920-DBFETCH3-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO   MCP-FUNC
           MOVE    "tbl_jyurrk"     TO   MCP-TABLE
           MOVE    "key21"          TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO           TO   FLG-JYURRK
             MOVE    MCPDATA-REC    TO   JYURRK-REC
      *
      *    テスト患者は対象外
             MOVE    JYURRK-PTID    TO   WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF   FLG-PTINF   =   ZERO
                 AND    PTINF-TSTPTNUMKBN  =  "1"
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
      ******************************************************************
      *    受診履歴情報クローズ処理
      ******************************************************************
       920-DBCLOSE3-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO   MCP-FUNC
           MOVE    "tbl_jyurrk"            TO   MCP-TABLE
           MOVE    "key21"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBCLOSE3-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD END   ----------------------------------------
      *****************************************************************
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE   WRK-PARA-HOSPNUM      TO   HKN-HOSPNUM
           MOVE   WRK-KOHNUM            TO   HKN-HKNNUM
           MOVE   HKNNUM-REC            TO   MCPDATA-REC
           MOVE   "DBSELECT"            TO   MCP-FUNC
           MOVE   "tbl_hknnum"          TO   MCP-TABLE
           MOVE   "key2"                TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE  "HKNNUM-KEY2"        TO   ORC-DBPATH
             PERFORM  955-HKNNUM-READ-SEC
      * 20080526 古い公費名を印字してしまう不具合を修正START tanaka
           ELSE
             MOVE     1                 TO   FLG-HKNNUM
      * 20080526 古い公費名を印字してしまう不具合を修正END
           END-IF
      *
      * 20080526 古い公費名を印字してしまう不具合を修正START tanaka
           PERFORM   UNTIL    FLG-HKNNUM  =   1
             IF   HKN-TEKSTYMD   <=   WRK-SRYYMD
                 AND   HKN-TEKEDYMD  >=  WRK-SRYYMD
               MOVE     1               TO   FLG-HKNNUM
             END-IF
      *
             IF   FLG-HKNNUM   =   ZERO
               MOVE    "tbl_hknnum"     TO   MCP-TABLE
               MOVE    "key2"           TO   MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
             END-IF
           END-PERFORM
      * 20080526 古い公費名を印字してしまう不具合を修正END
           INITIALIZE  MCPAREA
           MOVE   "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE   "tbl_hknnum"          TO   MCP-TABLE
           MOVE   "key2"                TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"           USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           MOVE   "DBFETCH"      TO   MCP-FUNC
           MOVE   "tbl_hknnum"   TO   MCP-TABLE
           MOVE   "key2"         TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"    USING
                                 MCPAREA
                                 MCPDATA-REC
                                 SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE   MCPDATA-REC  TO   HKNNUM-REC
             MOVE   ZERO         TO   FLG-HKNNUM
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE   1            TO   FLG-HKNNUM
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
           INITIALIZE                JOBKANRI-REC
           MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
           MOVE    S100LNK-JOBID     TO   JOB-JOBID
           MOVE    S100LNK-SHELLID   TO   JOB-SHELLID
           MOVE    CNT-PAGE          TO   JOB-UPDCNT
           CALL    "ORCSJOB"         USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
                                     SPA-AREA
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
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
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
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
