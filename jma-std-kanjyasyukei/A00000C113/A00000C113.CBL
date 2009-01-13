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
       PROGRAM-ID.             A00000C113.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : (医師別)入院患者数集計表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/01    @@@@-高橋     新規作成
      *  04/09/16    @@@@-本橋     MONPE化
      *  04/12/06    @@@@-本橋     有効期限が設定された医師コードにおいて
      *                            シス管理より医師名が取得できない不具合を修正
      *  05/03/01    @@@@-鈴木     open-cobol 0.23への対応による変更
      *  06/08/10    @@@@-高嶋     sarge対応
      *  07/09/12    @@@@-佐藤     4.0対応
      *  08/08/26    @@@@-田中     退院日を在院日にカウントしない不具合を修正  
      *  08/09/05    @@@@-田中     在院数の縦の加算合計を対応
      *                            担当医師がいない場合、該当医師なしに印字するよう修正
      *  08/10/20    @@@@-田中     在院数の集計条件の不具合を修正
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
      *****07/09/12  4.0対応  Satoh  START*****
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *****07/09/12  4.0対応  Satoh  END*******
      *
           COPY    "A00000C100N.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYKA-ALL       PIC 9(01).
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTNYUINRRK-M    PIC 9(01).
           03  FLG-PTNYUINRRK-M2   PIC 9(01).
           03  INP-STS             PIC 9(04).
           03  FLG-11DUMMY         PIC 9(01).
           03  FLG-12DUMMY         PIC 9(01).
           03  FLG-ZAIIN           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-DAY             PIC 9(02).
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
           03  SELKA-IDX         PIC 9(04).
           03  IDX1              PIC 9(04).
           03  IDX2              PIC 9(04).
           03  IDX3              PIC 9(04).
           03  IDX4              PIC 9(04).
           03  IDX5              PIC 9(04).
           03  IDX6              PIC 9(04).
           03  IDX7G.    
              05  IDX7           PIC 9(02).
           03  IDX8G.    
              05  IDX8           PIC 9(02).
           03  DIDXG.    
              05  DIDX           PIC 9(02).
           03  IDX9              PIC 9(04).
           03  IDX               PIC 9(04).
           03  IDY               PIC 9(04).
           03  IDZ               PIC 9(04).
           03  IDX-S             PIC 9(07).
           03  IDX-N             PIC 9(07).
           03  IDX-MAX           PIC 9(07).
      *
      **** 20050301 open-cobol 0.23への対応 SUZUKI
      *     03  IDX2              PIC 9(04).
      **** 20050301 SUZUKI
           03  JIHIIDX           PIC 9(04).
           03  NG                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH                PIC X(64).
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-YMD1WH            PIC X(22).
           03  WRK-YMD2WH            PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06). 
           03  WRK-SYUHKNNUM         PIC X(03). 
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
      *
      *****03  WRK-IN                PIC X(01).
      *
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-SYUEKI            PIC 9(08).
      *
           03  WRK-JIHI2            PIC 9(08).
           03  WRK-TJIHI-G       OCCURS  5.
               05  WRK-TJIHI        PIC X(08).
               05  WRK-TJIHI-FLG    PIC 9(01).
           03  WRK-TJIHIN-G       OCCURS  5.
               05  WRK-TJIHIN       PIC X(08).
               05  WRK-TJIHIN-FLG   PIC 9(01).
      *
           03  WRK-4001-TENTANKA PIC 9(02)V9(01).
      *
           03  WRK-RENNUM-X      PIC ZZ9.
           03  WRK-PAGE          PIC ZZ9.
           03  WRK-S9-3          PIC --9.
           03  WRK-Z9-5          PIC ZZ,ZZ9.
           03  WRK-Z9            PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ            PIC ZZZZZZZZZZZZ.
           03  WRK-S9            PIC -----------9.
           03  WRK-SZ            PIC -----------Z.
           03  WRK-NYUGAIKBN     PIC X(01).
           03  WRK-DRCD          PIC X(05).
           03  WRK-ZENDRCD       PIC X(05).
           03  WRK-SRYYMD        PIC X(08).
           03  WRK-SRYYM         PIC X(06).
           03  DEBUG-CNT         PIC 9(03).
           03  WRK-PR-SRYYM      PIC X(06). 
      *
           03  WRK-TMP-YMD.
            05 WRK-TMP-YY        PIC 9(04).
            05 WRK-TMP-MM        PIC 9(02).
            05 WRK-TMP-DD        PIC 9(02).
           03  WRK-TMP-LYMD.
            05 WRK-TMP-LYY       PIC 9(04).
            05 WRK-TMP-LMM       PIC 9(02).
            05 WRK-TMP-LDD       PIC 9(02).
      * 20080826 退院日を在院日にカウントしない不具合を修正START
           03  WRK-ZAINISSU      PIC  S9(05).
      * 20080826 退院日を在院日にカウントしない不具合を修正END
      *
       01  SUM-AREA                VALUE  ZERO.
           03  SUM-DRCDMAX        PIC    9(03).
           03  SUM-SRYYMDIMAX      PIC    9(05).
           03  SUM-SRYYMD-TBL      OCCURS 1000.
               05  SUM-SRYYMD      PIC    X(08).
           03  SUM-SRYKA-TBL       OCCURS 100.
               05  SUM-DRCD        PIC    X(05).
               05  SUM-DRNAME      PIC    X(20).
               05  SUM-SRYKA-ZAN   PIC   S9(05).
               05  SUM-ZENZAN-G.
                 07  SUM-ZENZAN-TBL    OCCURS  1000.
                   09  SUM-ZENZAN      PIC  S9(05).
               05  SUM-NYUIN-G.
                 07  SUM-NYUIN-TBL     OCCURS  1000.
                   09  SUM-NYUIN       PIC  S9(05).
               05  SUM-TAIIN-G.
                 07  SUM-TAIIN-TBL     OCCURS  1000.
                   09  SUM-TAIIN       PIC  S9(05).
               05  SUM-TOUZAN-G.
                 07  SUM-TOUZAN-TBL    OCCURS  1000.
                   09  SUM-TOUZAN      PIC  S9(05).
      * 20080826 退院日を在院日にカウントしない不具合を修正START
               05  SUM-TOUZAN2-G.
                 07  SUM-TOUZAN2-TBL    OCCURS  1000.
                   09  SUM-TOUZAN2      PIC  S9(05).
      * 20080826 退院日を在院日にカウントしない不具合を修正END
      * 20080905 在院数の縦の加算合計を対応START
           03  SUM-GZAIIN-G   OCCURS  1000.
               05  SUM-GZAIIN          PIC  S9(05).
               05  WRK-S9-5            PIC  S9(05).
      * 20080905 在院数の縦の加算合計を対応END
      
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *****07/09/12  4.0対応  Satoh  START*****
      *     COPY    "ORCA-DBPATH".         
      *****07/09/12  4.0対応  Satoh  END*******
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
      *    職員情報^M
           COPY    "CPSK1010.INC".
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
      *    労災情報取得
           COPY  "CPSK4001.INC".
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
      *    診療行為情報
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者入院履歴情報(入院履歴全枝番号用)
       01  PTNYUINRRK-M-REC.
           COPY    "CPPTNYUINRRK.INC"
           REPLACING  //PTNYUINRRK-// BY //PTNYUINRRK-M-//.
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
      *
           COPY    "A00000C100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000C100-LNK.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF  INP-STS  =   ZERO
      ***    正常主処理
             PERFORM 200-MAIN-SEC
           ELSE
      ***    エラー処理
             PERFORM 210-ERR-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
      *****07/09/12  4.0対応  Satoh  START*****
           INITIALIZE                  SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
      *
           PERFORM 100-DBOPEN-SEC
      *****07/09/12  4.0対応  Satoh  START*****
           MOVE    C100LNK-HOSPNUM TO  SPA-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*******
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *****07/09/12  4.0対応  Satoh  START*****
           MOVE    C100LNK-HOSPNUM TO  JOB-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*******
           MOVE    C100LNK-JOBID   TO  JOB-JOBID
           MOVE    C100LNK-SHELLID TO  JOB-SHELLID
           MOVE    "A00000C113"    TO  JOB-PGID
           MOVE    "(医師別)入院患者数集計表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                   SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
      *
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    C100LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      *****07/09/12  4.0対応  Satoh  END*******
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
      *    システム日付取得／和暦変換
           MOVE    C100LNK-PRTKANRI-SKYYMD TO WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH 
      *
      *    集計対象期間開始日取得／和暦変換
           MOVE    C100LNK-YMD1        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-YMD1WH 
      *
      *    集計対象期間開始日取得／和暦変換
           MOVE    C100LNK-YMD2        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-YMD2WH 
      *
      ***  画面入力値のチェック
      *
      *    集計区分のチェック
           IF   C100LNK-MDKBN   NOT =   SPACE
            AND C100LNK-MDKBN   NOT =   "0"
                MOVE    10      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    集計対象か期間のチェック
           IF   C100LNK-MDKBN   =   SPACE
             IF C100LNK-YMD1    >   C100LNK-YMD2
                MOVE    20      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
             END-IF
           ELSE 
             IF C100LNK-YMD1(1:6)   >   C100LNK-YMD2(1:6)
                MOVE    20      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
             END-IF
           END-IF
      *
           IF   C100LNK-YMD1(1:4)   NOT =   C100LNK-YMD2(1:4)
                MOVE    21      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    入外区分のチェック
           IF   C100LNK-NYUGAIKBN   NOT =   "1"
            AND C100LNK-NYUGAIKBN   NOT =   "2"
                MOVE    30      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *****07/09/12  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*******
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *****07/09/12  4.0対応  Satoh  START*****
      *         MOVE    SYS-1001-HOSPID     TO  C100LNK-HOSPID
               MOVE    SYS-1001-HOSPNUM    TO  C100LNK-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*******
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
      *
      *    医師別用テーブル作成
      *****07/09/12  4.0対応  Satoh  START*****
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
      *****07/09/12  4.0対応  Satoh  END*******
           MOVE    "1010"            TO   SYS-1010-KANRICD
      *****07/09/12  4.0対応  Satoh  START*****
           MOVE    SYS-1001-KBNCD      TO  SYS-1010-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*******
      ***  2004/12/06  MOTO
      ***     MOVE    C100LNK-YMD2      TO   WRK-SYMD
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    C100LNK-YMD2      TO   WRK-SYSYMD
           MOVE    C100LNK-YMD1      TO   SYS-1010-STYUKYMD
           MOVE    C100LNK-YMD2      TO   SYS-1010-EDYUKYMD
      *****07/09/12  4.0対応  Satoh  END*******
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE   1        TO     IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
      *    医者はKBNCDの頭１桁が"１"
                IF   FLG-SYSKANRI3 = ZERO AND
                     SYS-1010-KBNCD(1:1)  =  "1"
                     MOVE SYS-1010-KBNCD      TO SUM-DRCD(IDX1)
                     IF   SYS-1010-NAME NOT  = SPACE
                          MOVE SYS-1010-NAME      TO  SUM-DRNAME(IDX1)
                     ELSE
                          MOVE SYS-1010-KANANAME  TO  SUM-DRNAME(IDX1)
                     END-IF
                     ADD  1        TO     IDX1 
                 END-IF 
                 PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
      *=== sarge対応 ADD By Takashima(2006/08/10) start
           PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/08/10) end
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正START
           MOVE "ZZZZ"       TO   SUM-DRCD(IDX1)
           MOVE "該当医師なし"  TO   SUM-DRNAME(IDX1) 
           ADD  1        TO     IDX1 
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正END
           MOVE SPACE         TO   SUM-DRCD(IDX1)
           MOVE "全医師合計"  TO   SUM-DRNAME(IDX1) 
           MOVE  IDX1         TO   SUM-DRCDMAX 
      *
           IF  C100LNK-MDKBN   =   SPACE
      *      印刷年月日取得
             MOVE     1              TO   IDX1 
             MOVE     C100LNK-YMD1   TO   WRK-TMP-YMD
             INITIALIZE  WRK-SYMD
             MOVE    WRK-TMP-YMD     TO   WRK-SYMD(1:6)
             PERFORM 31012-LASTDAY-GET-SEC
             MOVE    WRK-SYMD        TO   WRK-TMP-LYMD
      *
             PERFORM  UNTIL  WRK-TMP-YMD  >   C100LNK-YMD2
                         OR  IDX1         >   1000 
              IF    WRK-TMP-YMD    >    WRK-TMP-LYMD    
                    IF WRK-TMP-MM  >=    12
                       ADD   1  TO WRK-TMP-YY
                       MOVE  1  TO WRK-TMP-MM
                       MOVE  1  TO WRK-TMP-DD
                    ELSE
                       ADD   1  TO WRK-TMP-MM
                       MOVE  1  TO WRK-TMP-DD
                    END-IF
                    INITIALIZE  WRK-SYMD
                    MOVE    WRK-TMP-YMD TO  WRK-SYMD(1:6)
                    PERFORM 31012-LASTDAY-GET-SEC
                    MOVE    WRK-SYMD    TO  WRK-TMP-LYMD
              END-IF 
              MOVE  WRK-TMP-YMD TO   SUM-SRYYMD(IDX1) 
              ADD   1           TO   WRK-TMP-DD
              ADD   1           TO   IDX1
             END-PERFORM
             MOVE     "計"        TO   SUM-SRYYMD(IDX1)  
             MOVE     IDX1        TO   SUM-SRYYMDIMAX   
           ELSE
      *      印刷年月取得
             MOVE   1                  TO   IDX1 
             MOVE   C100LNK-YMD1(1:6)  TO   WRK-TMP-YMD(1:6)
             MOVE   "01"               TO   WRK-TMP-YMD(7:2)
      *
             PERFORM UNTIL  WRK-TMP-YMD  >  C100LNK-YMD2
                        OR  IDX1         >  1000 
              IF    WRK-TMP-YMD    >    WRK-TMP-LYMD    
                    IF WRK-TMP-MM  >    12
                       ADD   1  TO WRK-TMP-YY
                       MOVE  1  TO WRK-TMP-MM
                    END-IF
              END-IF 
              MOVE  WRK-TMP-YMD(1:6) TO   SUM-SRYYMD(IDX1) 
              ADD   1                TO   WRK-TMP-MM
              ADD   1                TO   IDX1
             END-PERFORM
             MOVE     "計"        TO   SUM-SRYYMD(IDX1)  
             MOVE     IDX1        TO   SUM-SRYYMDIMAX   
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF  C100LNK-MDKBN   =   SPACE
      ***      (日別)入院数、退院数、在院中集計処理
               PERFORM 320-NYURRK-CNT-SEC
      *
      ***      (日別)入院数、退院数、在院中印刷処理
               PERFORM 330-TERM-HEN-SEC
           ELSE
      ***      (月別)入院数、退院数、在院中集計処理
               PERFORM 320-NYURRK-M-CNT-SEC
      *
      ***      (月別)入院数、退院数、在院中印刷処理
               PERFORM 330-TERM-M-HEN-SEC
           END-IF 
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値阿エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE     C100N
      *
           MOVE    WRK-SYSYMDWH   TO    C100N-PRTYMD
      *
           ADD     1              TO    CNT-PAGE
           MOVE    CNT-PAGE       TO    WRK-PAGE
           MOVE    WRK-PAGE       TO    C100N-PAGE
      *
           EVALUATE INP-STS 
           WHEN 10
             MOVE
             "入力された集計区分の値に誤りがあります"
                                  TO    C100N-JOBMSG
           WHEN 20
             MOVE
             "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    C100N-JOBMSG
           WHEN 21
             MOVE
             "入力する集計対象期間の範囲は同年内で指定をして下さい"
                                  TO    C100N-JOBMSG
           WHEN 30
             MOVE
             "入力された入外区分の値に誤りがあります"
                                  TO    C100N-JOBMSG
           END-EVALUATE
      *
           PERFORM 340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    (日別)入院数、退院数、在院中集計処理
      *****************************************************************
       320-NYURRK-CNT-SEC      SECTION.
      *
           MOVE SUM-DRCDMAX        TO  IDX3
           MOVE SUM-SRYYMDIMAX     TO  IDX5
      *
           INITIALIZE                      PTNYUINRRK-REC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    C100LNK-HOSPID      TO  PTNYUINRRK-HOSPID 
           MOVE    C100LNK-HOSPNUM      TO  PTNYUINRRK-HOSPNUM 
      *****07/09/12  4.0対応  Satoh  END*******
           MOVE    C100LNK-YMD2        TO  PTNYUINRRK-NYUINYMD
           MOVE    C100LNK-YMD1        TO  PTNYUINRRK-TAIINYMD
      *
           PERFORM  900-NYURRK-SELECT-SEC

           PERFORM  UNTIL   FLG-PTNYUINRRK  =   1

      ***      患者情報の取得
               INITIALIZE                   PTINF-REC 
      *****07/09/12  4.0対応  Satoh  START*****
      *         MOVE     PTNYUINRRK-HOSPID   TO  PTINF-HOSPID
               MOVE     PTNYUINRRK-HOSPNUM  TO  PTINF-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*******
               MOVE     PTNYUINRRK-PTID     TO  PTINF-PTID
               PERFORM  900-PTINF-READ-SEC

               IF  FLG-PTINF = ZERO
                 AND  PTINF-TSTPTNUMKBN  NOT  =  "1"
      *        患者情報にレコードが存在しないデータは抽出しない。
      *        テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う

      ***        入院履歴番号をKEYにして、入院履歴枝番号のMIN値から
      ***        対象となるレコード取得する(KEY5)
      * 20080905 退院日を在院日にカウントしない不具合を修正START
      *           INITIALIZE                      WRK-ZENDRCD
                 MOVE   99999   TO               WRK-ZENDRCD
      * 20080905 退院日を在院日にカウントしない不具合を修正END
                 INITIALIZE                      PTNYUINRRK-M-REC
                 MOVE    PTNYUINRRK-PTID     TO  PTNYUINRRK-M-PTID
      *****07/09/12  4.0対応  Satoh  START*****
      *           MOVE    C100LNK-HOSPID       TO  PTNYUINRRK-M-HOSPID 
                 MOVE    C100LNK-HOSPNUM      TO  PTNYUINRRK-M-HOSPNUM 
      *****07/09/12  4.0対応  Satoh  END*******
                 MOVE    PTNYUINRRK-RRKNUM   TO  PTNYUINRRK-M-RRKNUM
      *
                 PERFORM  900-NYURRK-M-SELECT-SEC
      *
                 PERFORM  UNTIL   FLG-PTNYUINRRK-M  =   1
      ***          指定期間(開始日)−初日在院者数の集計
                   IF    PTNYUINRRK-M-TENSTUYMD   >=   C100LNK-YMD1
                    AND PTNYUINRRK-M-TENNYUYMD    <   C100LNK-YMD1
                      MOVE     PTNYUINRRK-M-DRCD1  TO  WRK-DRCD
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正START
                      IF  WRK-DRCD  = SPACE
                          MOVE  "ZZZZ"   TO   WRK-DRCD
                      END-IF
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正END
                      PERFORM  320-DRCD-GET-IDX-SEC
                      ADD  1     TO  SUM-SRYKA-ZAN(IDX1)
                      ADD  1     TO  SUM-SRYKA-ZAN(IDX3)
                   END-IF
      ***  指定期間−入院者数の集計
      *    集計対象年月のデータであるかをチェックする。 
                   IF  (PTNYUINRRK-M-TENNYUYMD   >=  C100LNK-YMD1)
                    AND (PTNYUINRRK-M-TENNYUYMD  <=  C100LNK-YMD2) 
                    AND ( PTNYUINRRK-M-DRCD1  NOT = WRK-ZENDRCD) 
                      MOVE     PTNYUINRRK-M-DRCD1  TO  WRK-DRCD
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正START
                      IF  WRK-DRCD  = SPACE
                          MOVE  "ZZZZ"   TO   WRK-DRCD
                      END-IF
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正END
                      PERFORM  320-DRCD-GET-IDX-SEC
                      MOVE PTNYUINRRK-M-TENNYUYMD   TO  WRK-SRYYMD
                      PERFORM 320-SRYYMD-GET-IDX-SEC
                      ADD  1     TO  SUM-NYUIN(IDX1 IDX6)
                      ADD  1     TO  SUM-NYUIN(IDX1 IDX5)
                      ADD  1     TO  SUM-NYUIN(IDX3 IDX6)
                      ADD  1     TO  SUM-NYUIN(IDX3 IDX5)
                   END-IF
      ***  前回、医者コードの退避
                   MOVE    PTNYUINRRK-M-DRCD1  TO  WRK-ZENDRCD
                   PERFORM  900-NYURRK-M-FETCH-SEC
                 END-PERFORM
      *=== sarge対応 ADD By Takashima(2006/08/10) start
                 PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/08/10) end

      ***  指定期間−退院者数の集計
      *    集計対象年月のデータであるかをチェックする。 
      ***        入院履歴番号をKEYにして、入院履歴枝番号のMAX値から
      ***        対象となるレコード取得する(KEY4)
      * 20080905 退院日を在院日にカウントしない不具合を修正START
      *           INITIALIZE                      WRK-ZENDRCD
                 MOVE   99999   TO               WRK-ZENDRCD
      * 20080905 退院日を在院日にカウントしない不具合を修正END
                 INITIALIZE                      PTNYUINRRK-M-REC
                 MOVE    PTNYUINRRK-PTID     TO  PTNYUINRRK-M-PTID
      *****07/09/12  4.0対応  Satoh  START*****
      *           MOVE    C100LNK-HOSPID      TO  PTNYUINRRK-M-HOSPID 
                 MOVE    C100LNK-HOSPNUM      TO  PTNYUINRRK-M-HOSPNUM 
      *****07/09/12  4.0対応  Satoh  END*******
                 MOVE    PTNYUINRRK-RRKNUM   TO  PTNYUINRRK-M-RRKNUM
      *
                 PERFORM  900-NYURRK-M2-SELECT-SEC
      *
                 PERFORM  UNTIL   FLG-PTNYUINRRK-M2  =   1
                   IF  (PTNYUINRRK-M-TENSTUYMD   >=  C100LNK-YMD1)
                    AND (PTNYUINRRK-M-TENSTUYMD  <=  C100LNK-YMD2) 
                    AND ( PTNYUINRRK-M-DRCD1  NOT = WRK-ZENDRCD ) 
                      MOVE     PTNYUINRRK-M-DRCD1  TO  WRK-DRCD
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正START
                      IF  WRK-DRCD  = SPACE
                          MOVE  "ZZZZ"   TO   WRK-DRCD
                      END-IF
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正END
                      PERFORM  320-DRCD-GET-IDX-SEC
                      MOVE PTNYUINRRK-M-TENSTUYMD   TO  WRK-SRYYMD
                      PERFORM 320-SRYYMD-GET-IDX-SEC
                      ADD  1     TO  SUM-TAIIN(IDX1 IDX6)
                      ADD  1     TO  SUM-TAIIN(IDX1 IDX5)
                      ADD  1     TO  SUM-TAIIN(IDX3 IDX6)
                      ADD  1     TO  SUM-TAIIN(IDX3 IDX5)
      *--- 2008/10/20  ADD  START ------------------------------------
                      ADD  1     TO  SUM-TOUZAN2(IDX1 IDX6)
                      ADD  1     TO  SUM-TOUZAN2(IDX3 IDX6)
      *--- 2008/10/20  ADD  END ------------------------------------
                   END-IF
      * 20080826 退院日を在院日にカウントしない不具合を修正START
      *--- 2008/10/20  DELETE  START ------------------------------------
      *             IF  PTNYUINRRK-M-TENSTUYMD = PTNYUINRRK-M-TAIINYMD
      *              AND (PTNYUINRRK-M-TENSTUYMD  >=  C100LNK-YMD1)
      *              AND (PTNYUINRRK-M-TENSTUYMD  <=  C100LNK-YMD2) 
      *                ADD  1     TO  SUM-TOUZAN2(IDX1 IDX6)
      *                ADD  1     TO  SUM-TOUZAN2(IDX3 IDX6)
      *            END-IF
      *--- 2008/10/20  DELETE  END ------------------------------------
      * 20080826 退院日を在院日にカウントしない不具合を修正END
      ***  前回、医者コードの退避
                   MOVE    PTNYUINRRK-M-DRCD1      TO  WRK-ZENDRCD
                   PERFORM  900-NYURRK-M2-FETCH-SEC
                 END-PERFORM
      *=== sarge対応 ADD By Takashima(2006/08/10) start
                 PERFORM 900-M2-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/08/10) end
               END-IF
               PERFORM  900-NYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   900-NYURRK-CLOSE-SEC
      *
           .
       320-NYURRK-CNT-EXT.
           EXIT.
      *****************************************************************
      *    (月別)入院数、退院数、在院中集計処理
      *****************************************************************
       320-NYURRK-M-CNT-SEC      SECTION.
      *
           MOVE SUM-DRCDMAX       TO  IDX3
           MOVE SUM-SRYYMDIMAX     TO  IDX5
      *
           INITIALIZE                  PTNYUINRRK-REC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    C100LNK-HOSPID      TO  PTNYUINRRK-HOSPID 
           MOVE    C100LNK-HOSPNUM      TO  PTNYUINRRK-HOSPNUM 
      *****07/09/12  4.0対応  Satoh  END*******
           MOVE    C100LNK-YMD2(1:6)   TO  PTNYUINRRK-NYUINYMD(1:6) 
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD(7:2) 

           MOVE    C100LNK-YMD1(1:6)   TO  PTNYUINRRK-TAIINYMD(1:6) 
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD(7:2) 

           PERFORM  900-NYURRK-SELECT-SEC

           PERFORM  UNTIL   FLG-PTNYUINRRK  =   1
      ***      患者情報の取得
               INITIALIZE                   PTINF-REC 
      *****07/09/12  4.0対応  Satoh  START*****
      *         MOVE     PTNYUINRRK-HOSPID   TO  PTINF-HOSPID
               MOVE     PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*****
               MOVE     PTNYUINRRK-PTID     TO  PTINF-PTID
               PERFORM  900-PTINF-READ-SEC

               IF  FLG-PTINF = ZERO
                 AND  PTINF-TSTPTNUMKBN  NOT  =  "1"
      *        患者情報にレコードが存在しないデータは抽出しない。
      *        テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う

      ***        入院履歴番号をKEYにして、入院履歴枝番号のMIN値から
      ***        対象となるレコード取得する(KEY5)
      * 20080905 退院日を在院日にカウントしない不具合を修正START
      *           INITIALIZE                      WRK-ZENDRCD
                 MOVE   99999   TO               WRK-ZENDRCD
      * 20080905 退院日を在院日にカウントしない不具合を修正END
                 INITIALIZE                      PTNYUINRRK-M-REC
                 MOVE    PTNYUINRRK-PTID     TO  PTNYUINRRK-M-PTID
      *****07/09/12  4.0対応  Satoh  START*****
      *           MOVE    C100LNK-HOSPID      TO  PTNYUINRRK-M-HOSPID 
                 MOVE    C100LNK-HOSPNUM      TO  PTNYUINRRK-M-HOSPNUM 
      *****07/09/12  4.0対応  Satoh  END*******
                 MOVE    PTNYUINRRK-RRKNUM   TO  PTNYUINRRK-M-RRKNUM
      *
                 PERFORM  900-NYURRK-M-SELECT-SEC
      *
                 PERFORM  UNTIL   FLG-PTNYUINRRK-M  =   1
      ***          指定期間(開始日)−月初在院者数の集計
                   IF  PTNYUINRRK-M-TENSTUYMD(1:6) >= C100LNK-YMD1(1:6)
                    AND PTNYUINRRK-M-TENNYUYMD(1:6) < C100LNK-YMD1(1:6)
                      MOVE     PTNYUINRRK-M-DRCD1  TO  WRK-DRCD
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正START
                      IF  WRK-DRCD  = SPACE
                          MOVE  "ZZZZ"   TO   WRK-DRCD
                      END-IF
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正END
                      PERFORM  320-DRCD-GET-IDX-SEC
                      ADD  1     TO  SUM-SRYKA-ZAN(IDX1)
                      ADD  1     TO  SUM-SRYKA-ZAN(IDX3)
                   END-IF

      ***  指定期間−入院者数の集計
      *    集計対象年月のデータであるかをチェックする。 
                   IF (PTNYUINRRK-M-TENNYUYMD(1:6) >= C100LNK-YMD1(1:6))
                  AND (PTNYUINRRK-M-TENNYUYMD(1:6) <= C100LNK-YMD2(1:6))
                  AND ( PTNYUINRRK-M-DRCD1 NOT = WRK-ZENDRCD ) 
                      MOVE     PTNYUINRRK-M-DRCD1  TO  WRK-DRCD
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正START
                      IF  WRK-DRCD  = SPACE
                          MOVE  "ZZZZ"   TO   WRK-DRCD
                      END-IF
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正END
                      PERFORM  320-DRCD-GET-IDX-SEC
                      MOVE PTNYUINRRK-M-TENNYUYMD(1:6)  TO  WRK-SRYYM
                      PERFORM 320-SRYYM-GET-IDX-SEC
                      ADD  1     TO  SUM-NYUIN(IDX1 IDX6)
                      ADD  1     TO  SUM-NYUIN(IDX1 IDX5)
                      ADD  1     TO  SUM-NYUIN(IDX3 IDX6)
                      ADD  1     TO  SUM-NYUIN(IDX3 IDX5)
                   END-IF
      ***  前回、医者コードの退避
                   MOVE    PTNYUINRRK-M-DRCD1  TO  WRK-ZENDRCD
                   PERFORM  900-NYURRK-M-FETCH-SEC
                 END-PERFORM
      *=== sarge対応 ADD By Takashima(2006/08/10) start
                 PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/08/10) end

      ***  指定期間−退院者数の集計
      *    集計対象年月のデータであるかをチェックする。 
      ***        入院履歴番号をKEYにして、入院履歴枝番号のMAX値から
      ***        対象となるレコード取得する(KEY4)
      * 20080905 退院日を在院日にカウントしない不具合を修正START
      *           INITIALIZE                      WRK-ZENDRCD
                 MOVE   99999   TO               WRK-ZENDRCD
      * 20080905 退院日を在院日にカウントしない不具合を修正END
                 INITIALIZE                      PTNYUINRRK-M-REC
                 MOVE    PTNYUINRRK-PTID     TO  PTNYUINRRK-M-PTID
      *****07/09/12  4.0対応  Satoh  START*****
      *           MOVE    C100LNK-HOSPID      TO  PTNYUINRRK-M-HOSPID 
                 MOVE    C100LNK-HOSPNUM      TO  PTNYUINRRK-M-HOSPNUM 
      *****07/09/12  4.0対応  Satoh  END*******
                 MOVE    PTNYUINRRK-RRKNUM   TO  PTNYUINRRK-M-RRKNUM
      *
                 PERFORM  900-NYURRK-M2-SELECT-SEC
      *
                 PERFORM  UNTIL   FLG-PTNYUINRRK-M2  =   1
                   IF (PTNYUINRRK-M-TENSTUYMD(1:6) >= C100LNK-YMD1(1:6))
                  AND (PTNYUINRRK-M-TENSTUYMD(1:6) <= C100LNK-YMD2(1:6))
                  AND ( PTNYUINRRK-M-DRCD1 NOT = WRK-ZENDRCD ) 
                      MOVE     PTNYUINRRK-M-DRCD1  TO  WRK-DRCD
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正START
                      IF  WRK-DRCD  = SPACE
                          MOVE  "ZZZZ"   TO   WRK-DRCD
                      END-IF
      * 20080905 担当医師がいない場合、該当医師なしに印字するよう修正END
                      PERFORM  320-DRCD-GET-IDX-SEC
                      MOVE PTNYUINRRK-M-TENSTUYMD(1:6)  TO  WRK-SRYYM
                      PERFORM 320-SRYYM-GET-IDX-SEC
                      ADD  1     TO  SUM-TAIIN(IDX1 IDX6)
                      ADD  1     TO  SUM-TAIIN(IDX1 IDX5)
                      ADD  1     TO  SUM-TAIIN(IDX3 IDX6)
                      ADD  1     TO  SUM-TAIIN(IDX3 IDX5)
      *--- 2008/10/20  ADD  START ------------------------------------
                      ADD  1     TO  SUM-TOUZAN2(IDX1 IDX6)
                      ADD  1     TO  SUM-TOUZAN2(IDX3 IDX6)
      *--- 2008/10/20  ADD  END ------------------------------------
                   END-IF
      * 20080826 退院日を在院日にカウントしない不具合を修正START
      *--- 2008/10/20  DELETE START ------------------------------------
      *             IF  PTNYUINRRK-M-TENSTUYMD = PTNYUINRRK-M-TAIINYMD
      *               AND (PTNYUINRRK-M-TENSTUYMD  >=  C100LNK-YMD1)
      *               AND (PTNYUINRRK-M-TENSTUYMD  <=  C100LNK-YMD2) 
      *                 ADD  1     TO  SUM-TOUZAN2(IDX1 IDX6)
      *                 ADD  1     TO  SUM-TOUZAN2(IDX3 IDX6)
      *             END-IF
      *--- 2008/10/20  DELETE END ------------------------------------
      * 20080826 退院日を在院日にカウントしない不具合を修正END
      ***  前回、医者コードの退避
                   MOVE    PTNYUINRRK-M-DRCD1  TO  WRK-ZENDRCD
                   PERFORM  900-NYURRK-M2-FETCH-SEC
                 END-PERFORM
      *=== sarge対応 ADD By Takashima(2006/08/10) start
                 PERFORM 900-M2-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/08/10) end
               END-IF
               PERFORM  900-NYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   900-NYURRK-CLOSE-SEC
      *
           .
       320-NYURRK-M-CNT-EXT.
           EXIT.
      *****************************************************************
      *    医師INDEX判別処理
      *****************************************************************
       320-DRCD-GET-IDX-SEC      SECTION.
      *
             MOVE  1               TO  IDX1
             MOVE  SUM-DRCDMAX     TO  IDX2 
             PERFORM  UNTIL  IDX1   >  IDX2
                      IF    SUM-DRCD(IDX1)  = WRK-DRCD
                            MOVE ZERO  TO      IDX2
                      ELSE
                            ADD   1    TO      IDX1
                      END-IF
             END-PERFORM
      *
           .
       320-DRCD-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日INDEX判定処理
      *****************************************************************
       320-SRYYMD-GET-IDX-SEC      SECTION.
      *
             MOVE  1               TO  IDX6
             MOVE  SUM-SRYYMDIMAX  TO  IDX2 
             PERFORM  UNTIL  IDX6   >  IDX2
                      IF    SUM-SRYYMD(IDX6)  = WRK-SRYYMD
                            MOVE ZERO  TO       IDX2
                      ELSE 
                            ADD  1     TO       IDX6
                      END-IF
             END-PERFORM
      *
           .
       320-SRYYMD-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療月INDEX判定処理
      *****************************************************************
       320-SRYYM-GET-IDX-SEC      SECTION.
      *
             MOVE  1               TO  IDX6
             MOVE  SUM-SRYYMDIMAX  TO  IDX2 
             PERFORM  UNTIL  IDX6   >  IDX2
                      IF    SUM-SRYYMD(IDX6)(1:6)  = WRK-SRYYM
                            MOVE ZERO  TO       IDX2
                      ELSE 
                            ADD  1     TO       IDX6
                      END-IF
             END-PERFORM
      *
           .
       320-SRYYM-GET-IDX-EXT.
           EXIT.
      *
      *****************************************************************
      *    (日別)入院数、退院数、在院中印刷処理
      *****************************************************************
       330-TERM-HEN-SEC         SECTION.
      *
           MOVE       1       TO      IDX5  
           PERFORM    UNTIL   IDX5    >     SUM-SRYYMDIMAX    
             MOVE       1       TO      IDX2  
             PERFORM    UNTIL   IDX2    >     SUM-DRCDMAX    
      *
               INITIALIZE     C100N
      *
      * 帳票タイトル
               MOVE "(医師別)  入  院  患  者  数  集  計  表"
                                      TO    C100N-MIDASHI
      * 出力年月日
               MOVE    WRK-SYSYMDWH   TO    C100N-PRTYMD
      * 集計対象期間開始日
               MOVE    WRK-YMD1WH     TO    C100N-SRYMD-FR  
      * 集計対象期間終了日
               MOVE    WRK-YMD2WH(11:)
                                      TO    C100N-SRMD-TO 
      * 集計単位 
               MOVE    "月  日"       TO    C100N-TANI  
      *
               ADD     1              TO    CNT-PAGE
               MOVE    CNT-PAGE       TO    WRK-PAGE
               MOVE    WRK-PAGE       TO    C100N-PAGE
      *
               MOVE    1              TO    IDX3  
               PERFORM  UNTIL   IDX3 >      7
                           OR   IDX2 >      SUM-DRCDMAX    
      * 20080905 在院数の縦の加算合計を対応
                INITIALIZE  SUM-GZAIIN(IDX3)
      * 20080905 在院数の縦の加算合計を対応
      * 診療科名称を集計エリアに転送
                MOVE    SUM-DRNAME(IDX2)
                                       TO  C100N-SRYKA(IDX3)
      * 件数名称を転送
                MOVE    "入  院"   TO  C100N-KENSU-1(IDX3)
                MOVE    "(変更 含む)"   TO  C100N-TENKA-1(IDX3)
                MOVE    "退  院"   TO  C100N-KENSU-2(IDX3)
                MOVE    "(変更 含む)"   TO  C100N-TENKA-2(IDX3)
                MOVE    "在院中"       TO  C100N-KENSU-3(IDX3)
      *
      * 初日の在院者数を集計エリアに転送
           IF   FLG-ZAIIN =  ZERO
                MOVE    SUM-SRYKA-ZAN(IDX2) TO  SUM-ZENZAN(IDX2 IDX5)
           END-IF
      *
      * 日別集計数の集計値を転送
                MOVE  1                TO    IDX6
                MOVE  IDX5             TO    IDX4
                MOVE  SUM-SRYYMD(IDX4)(1:6)
                                       TO    WRK-PR-SRYYM 
                PERFORM UNTIL IDX4  >  SUM-SRYYMDIMAX
                        OR    IDX6  >  32
                        OR  ( SUM-SRYYMD(IDX4)      NOT =
                              "計"
                         AND  SUM-SRYYMD(IDX4)(1:6) NOT =
                              WRK-PR-SRYYM )
                 IF IDX4   =     SUM-SRYYMDIMAX
                  MOVE  "計"     TO  C100N-MD(32)

                  MOVE  SUM-NYUIN(IDX2 IDX4) TO  WRK-Z9-5
                  MOVE  WRK-Z9-5 TO  C100N-11-CNT(IDX3 32)
                  MOVE  SUM-TAIIN(IDX2 IDX4) TO  WRK-Z9-5
                  MOVE  WRK-Z9-5 TO  C100N-12-CNT(IDX3 32)
      * 20080905 在院数の縦の加算合計を対応START
      *            MOVE  SPACE    TO  C100N-99-CNT(IDX3 32)
                  MOVE  SUM-GZAIIN(IDX3)  TO  WRK-Z9-5
                  MOVE  WRK-Z9-5  TO  C100N-99-CNT(IDX3 32)
      * 20080905 在院数の縦の加算合計を対応END
                 ELSE                
                  MOVE  SUM-SRYYMD(IDX4)(5:2)  
                                 TO  C100N-MD(IDX6)(1:2)
                  MOVE  "/"      TO  C100N-MD(IDX6)(3:1)
                  MOVE  SUM-SRYYMD(IDX4)(7:2)
                                 TO  C100N-MD(IDX6)(4:2)

                  MOVE  SUM-NYUIN(IDX2 IDX4)  TO  WRK-Z9-5
                  MOVE  WRK-Z9-5 TO  C100N-11-CNT(IDX3 IDX6)
                  MOVE  SUM-TAIIN(IDX2 IDX4)  TO  WRK-Z9-5
                  MOVE  WRK-Z9-5 TO  C100N-12-CNT(IDX3 IDX6)
                  COMPUTE  SUM-TOUZAN(IDX2 IDX4) =
                           SUM-ZENZAN(IDX2 IDX4) 
                         + SUM-NYUIN(IDX2 IDX4)
                         - SUM-TAIIN(IDX2 IDX4)
      * 20080826 退院日を在院日にカウントしない不具合を修正START
                  MOVE   SUM-TOUZAN(IDX2 IDX4)  TO    WRK-ZAINISSU 
                  ADD    SUM-TOUZAN2(IDX2 IDX4)  TO    WRK-ZAINISSU
                  MOVE   WRK-ZAINISSU           TO    WRK-Z9-5
      *            MOVE  SUM-TOUZAN(IDX2 IDX4) TO  WRK-Z9-5
      * 20080826 退院日を在院日にカウントしない不具合を修正END
                  MOVE  WRK-Z9-5 TO  C100N-99-CNT(IDX3 IDX6)
      * 20080905 在院数の縦の加算合計を対応START
                  MOVE  WRK-Z9-5    TO   WRK-S9-5
                  ADD   WRK-S9-5    TO   SUM-GZAIIN(IDX3)
      * 20080905 在院数の縦の加算合計を対応END
                  COMPUTE   IDX9 =   IDX4     +   1  
                  MOVE  SUM-TOUZAN(IDX2 IDX4) 
                        TO           SUM-ZENZAN(IDX2 IDX9)

                 END-IF
                 ADD    1        TO  IDX4
                 ADD    1        TO  IDX6
                END-PERFORM
      *
                ADD     1                   TO  IDX2  
                ADD     1                   TO  IDX3  
               END-PERFORM
      *
               PERFORM 340-PRINT-OUT-SEC
      * 
             END-PERFORM
             COMPUTE    IDX5    =   IDX5    +  (IDX6 - 1)    
             MOVE    1       TO     FLG-ZAIIN
           END-PERFORM
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    (月別)入院数、退院数、在院中印刷処理
      *****************************************************************
       330-TERM-M-HEN-SEC         SECTION.
      *
           MOVE       1       TO      IDX5  
           PERFORM    UNTIL   IDX5    >     SUM-SRYYMDIMAX    
             MOVE       1       TO      IDX2  
             PERFORM    UNTIL   IDX2    >     SUM-DRCDMAX    
      *
               INITIALIZE     C100N
      *
      * 帳票タイトル
               MOVE "(医師別)  入  院  患  者  数  集  計  表"
                                      TO    C100N-MIDASHI
      * 出力年月日
               MOVE    WRK-SYSYMDWH   TO    C100N-PRTYMD
      * 集計対象期間開始日
               MOVE    WRK-YMD1WH(1:16)
                                      TO    C100N-SRYMD-FR  
      * 集計対象期間終了日
               MOVE    WRK-YMD2WH(11:6)
                                      TO    C100N-SRMD-TO 
      * 集計単位 
               MOVE    "  月"         TO    C100N-TANI  
      *
               ADD     1              TO    CNT-PAGE
               MOVE    CNT-PAGE       TO    WRK-PAGE
               MOVE    WRK-PAGE       TO    C100N-PAGE
      *
               MOVE    1              TO    IDX3  
               PERFORM  UNTIL   IDX3 >      7
                           OR   IDX2 >      SUM-DRCDMAX    
      * 20080905 在院数の縦の加算合計を対応
                INITIALIZE  SUM-GZAIIN(IDX3)
      * 20080905 在院数の縦の加算合計を対応
      * 診療科名称を転送
                MOVE    SUM-DRNAME(IDX2)
                                       TO  C100N-SRYKA(IDX3)
      * 件数名称を転送
                MOVE    "入  院"   TO  C100N-KENSU-1(IDX3)
                MOVE    "(変更 含む)"   TO  C100N-TENKA-1(IDX3)
                MOVE    "退  院"   TO  C100N-KENSU-2(IDX3)
                MOVE    "(変更 含む)"   TO  C100N-TENKA-2(IDX3)
                MOVE    "在院中"       TO  C100N-KENSU-3(IDX3)
      *
      * 月初の在院者数を集計エリアに転送
           IF   FLG-ZAIIN =  ZERO
                MOVE    SUM-SRYKA-ZAN(IDX2) TO  SUM-ZENZAN(IDX2 IDX5)
           END-IF
      *
      * 月別集計数の集計値を転送
                MOVE  1                TO    IDX6
                MOVE  IDX5             TO    IDX4
                PERFORM UNTIL IDX4  >  SUM-SRYYMDIMAX
                        OR    IDX6  >  32
                 IF IDX4   =     SUM-SRYYMDIMAX
                  MOVE  "計"     TO  C100N-MD(32)
                  MOVE  SUM-NYUIN(IDX2 IDX4) TO  WRK-Z9-5
                  MOVE  WRK-Z9-5 TO  C100N-11-CNT(IDX3 32)
                  MOVE  SUM-TAIIN(IDX2 IDX4) TO  WRK-Z9-5
                  MOVE  WRK-Z9-5 TO  C100N-12-CNT(IDX3 32)
      * 20080905 在院数の縦の加算合計を対応START
      *            MOVE  SPACE    TO  C100N-99-CNT(IDX3 32)
                  MOVE  SUM-GZAIIN(IDX3)  TO  WRK-Z9-5
                  MOVE  WRK-Z9-5  TO  C100N-99-CNT(IDX3 32)
      * 20080905 在院数の縦の加算合計を対応END
                 ELSE                
                  MOVE  SUM-SRYYMD(IDX4)(5:2)  
                                 TO  C100N-MD(IDX6)(4:2)
                  MOVE  SUM-NYUIN(IDX2 IDX4)  TO  WRK-Z9-5
                  MOVE  WRK-Z9-5 TO  C100N-11-CNT(IDX3 IDX6)
                  MOVE  SUM-TAIIN(IDX2 IDX4)  TO  WRK-Z9-5
                  MOVE  WRK-Z9-5 TO  C100N-12-CNT(IDX3 IDX6)
                  COMPUTE  SUM-TOUZAN(IDX2 IDX4) =
                           SUM-ZENZAN(IDX2 IDX4) 
                         + SUM-NYUIN(IDX2 IDX4)
                         - SUM-TAIIN(IDX2 IDX4)
      * 20080826 退院日を在院日にカウントしない不具合を修正START
                  MOVE   SUM-TOUZAN(IDX2 IDX4)  TO    WRK-ZAINISSU 
                  ADD    SUM-TOUZAN2(IDX2 IDX4) TO    WRK-ZAINISSU
                  MOVE   WRK-ZAINISSU           TO    WRK-Z9-5
      *            MOVE  SUM-TOUZAN(IDX2 IDX4) TO  WRK-Z9-5
      * 20080826 退院日を在院日にカウントしない不具合を修正END
                  MOVE  WRK-Z9-5 TO  C100N-99-CNT(IDX3 IDX6)
      * 20080905 在院数の縦の加算合計を対応START
                  MOVE  WRK-Z9-5 TO  WRK-S9-5
                  ADD   WRK-S9-5 TO  SUM-GZAIIN(IDX3)
      * 20080905 在院数の縦の加算合計を対応END
                  COMPUTE   IDX9 =   IDX4     +   1  
                  MOVE  SUM-TOUZAN(IDX2 IDX4) 
                        TO           SUM-ZENZAN(IDX2 IDX9)
                 END-IF
                 ADD    1        TO  IDX4
                 ADD    1        TO  IDX6
                END-PERFORM
      *
                ADD     1                   TO  IDX2  
                ADD     1                   TO  IDX3  
               END-PERFORM
      *
               PERFORM 340-PRINT-OUT-SEC
      * 
             END-PERFORM
             COMPUTE    IDX5    =   IDX5    +  (IDX6 - 1)    
             MOVE    1       TO     FLG-ZAIIN
           END-PERFORM
      *
           .
       330-TERM-M-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *    

               INITIALIZE                  ORCSPRTAREA
               MOVE    "INS"               TO  SPRT-MODE
               MOVE    C100LNK-PRTKANRI-RENNUM
                                           TO  SPRT-RENNUM
               MOVE    C100LNK-PRTKANRI-TBL-KEY
                                           TO  SPRT-TBL-KEY
               MOVE    C100LNK-PRTKANRI-TBL-GROUP
                                           TO  SPRT-TBL-GROUP
               MOVE    C100LNK-PRTKANRI-SRYYM
                                           TO  SPRT-SRYYM
               MOVE    C100LNK-PRTKANRI-SKYYMD
                                           TO  SPRT-SKYYMD
               MOVE    C100LNK-PRTKANRI-SHELLID 
                                           TO  SPRT-SHELLID
               MOVE    C100LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SPRT-SHORI-RENNUM
               MOVE    C100LNK-PRTKANRI-PRIORITY
                                           TO  SPRT-PRIORITY
      ***  20040916 MOTOHASI MONPEに変更
      ***         MOVE   "A00000C100N"         TO  SPRT-PRTID
               MOVE   "A00000C100N.red"         TO  SPRT-PRTID
      ***  20040916 MOTOHASI MONPEに変更
               MOVE   "(医師別)入院患者数集計表"
                                           TO  SPRT-TITLE
               MOVE    C100N                TO  SPRT-PRTDATA
               MOVE    C100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
               MOVE    C100LNK-PRTKANRI-OPID   TO  SPRT-OPID
               MOVE    C100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***  site-libを実行環境にする為コメントアウト
      ***         MOVE    "1"          TO  SPRT-SITEKBN
               CALL    "ORCSPRT"         USING
                                         ORCSPRTAREA
      *****07/09/12  4.0対応  Satoh  START*****
                                         SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
               IF  SPRT-RETURN         =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC                           
               END-IF
      *
           .
       340-PRINT-OUT-EXT.
           EXIT.
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
      *****07/09/12  4.0対応  Satoh  START*****
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*******
               MOVE    C100LNK-JOBID   TO  JOB-JOBID
               MOVE    C100LNK-SHELLID TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *****07/09/12  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *****07/09/12  4.0対応  Satoh  END*******
           MOVE    C100LNK-JOBID   TO  JOB-JOBID
           MOVE    C100LNK-SHELLID TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                   SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
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
      *    入院履歴情報検索
      *****************************************************************
       900-NYURRK-SELECT-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "PTNYUINRRK-KEY26"  TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"            USING
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key26"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
      *****07/09/12  4.0対応  Satoh  END*****
                                           MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                     ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
          IF      MCP-RC           =   ZERO
               PERFORM 900-NYURRK-FETCH-SEC
           ELSE
               MOVE    1           TO    FLG-PTNYUINRRK
               MOVE    1           TO    FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/10) start
      *         PERFORM 900-NYURRK-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/10) end
           END-IF        
           .
       900-NYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索
      *****************************************************************
       900-NYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "PTNYUINRRK-KEY26" TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"            USING
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key26"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
      *****07/09/12  4.0対応  Satoh  END*****
                                          MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                    ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                          MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK
               MOVE    MCPDATA-REC  TO  PTNYUINRRK-REC
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK
               MOVE    1            TO  FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/10) start
      *         PERFORM 900-NYURRK-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/10) end
           END-IF  
           .
       900-NYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-NYURRK-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE  "PTNYUINRRK-KEY26" TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"            USING
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key26"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           .
       900-NYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報検索(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
       900-NYURRK-M-SELECT-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-M-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "PTNYUINRRK-KEY5"       TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"            USING
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                     ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                           SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
          IF      MCP-RC           =   ZERO
               PERFORM 900-NYURRK-M-FETCH-SEC
           ELSE
               MOVE    1           TO    FLG-PTNYUINRRK-M
               MOVE    1           TO    FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/10) start
      *         PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/10) end
           END-IF        
           .
       900-NYURRK-M-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
       900-NYURRK-M-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "PTNYUINRRK-KEY5" TO   ORC-DBPATH
      *     CALL    "ORCMCPSUB"            USING
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key5"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
      *****07/09/12  4.0対応  Satoh  END*******
                                          MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                    ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                          MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                          SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK-M
               MOVE    MCPDATA-REC  TO  PTNYUINRRK-M-REC
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK-M
               MOVE    1            TO  FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/10) start
      *         PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/10) end
           END-IF  
           .
       900-NYURRK-M-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
       900-M-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE  "PTNYUINRRK-KEY5" TO   ORC-DBPATH
      *     CALL  "ORCMCPSUB"            USING
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           .
       900-M-DBCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院履歴情報検索(入院履歴枝番号のMAX値取得:KEY4)
      *****************************************************************
       900-NYURRK-M2-SELECT-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-M-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "PTNYUINRRK-KEY4"   TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"        USING
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                 ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
          IF      MCP-RC           =   ZERO
               PERFORM 900-NYURRK-M2-FETCH-SEC
           ELSE
               MOVE    1           TO    FLG-PTNYUINRRK-M2
               MOVE    1           TO    FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/10) start
      *         PERFORM 900-M2-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/10) end
           END-IF        
           .
       900-NYURRK-M2-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索(入院履歴枝番号のMAX値取得:KEY4)
      *****************************************************************
       900-NYURRK-M2-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "PTNYUINRRK-KEY4"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"        USING
           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
           MOVE    "key4"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      *****07/09/12  4.0対応  Satoh  END*******
                                      MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK-M2
               MOVE    MCPDATA-REC  TO  PTNYUINRRK-M-REC
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK-M2
               MOVE    1            TO  FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/10) start
      *         PERFORM 900-M2-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/10) end
           END-IF  
           .
       900-NYURRK-M2-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理(入院履歴枝番号のMAX値取得:KEY4)
      *****************************************************************
       900-M2-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE  "PTNYUINRRK-KEY4"  TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"        USING
           MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
           MOVE    "key4"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
      *****07/09/12  4.0対応  Satoh  END*******
                                    MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                              ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                    MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                    SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           .
       900-M2-DBCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SRYACCT-KEY3"    TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"       USING
           MOVE    "tbl_sryacct"     TO  MCP-TABLE
           MOVE    "key3"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
      *****07/09/12  4.0対応  Satoh  END*******
                                     MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                               ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                     MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                      SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"             TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *         MOVE    "SRYACCT-KEY3"    TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"       USING
               MOVE    "tbl_sryacct"     TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
      *****07/09/12  4.0対応  Satoh  END*******
                                         MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                   ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                         MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                         SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
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
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *         MOVE    "SRYACCT-KEY3"    TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"       USING
               MOVE    "tbl_sryacct"     TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
      *****07/09/12  4.0対応  Satoh  END*******
                                         MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                   ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                         MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                         SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-SELECT-SEC               SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"        USING
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                 ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
          IF      MCP-RC           =   ZERO
               MOVE    ZERO        TO  FLG-SRYACT
           ELSE
               MOVE    1           TO  FLG-SRYACT
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
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBFETCH"            TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SRYACT-KEY2"        TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-SRYACT
               MOVE    MCPDATA-REC  TO  SRYACT-REC
           ELSE
               MOVE    1            TO  FLG-SRYACT
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
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE  "SRYACT-KEY2"        TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_sryact"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      *****07/09/12  4.0対応  Satoh  END*******
                                      MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                      MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                      SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                 ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *         MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"         USING
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                     ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                           SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                 ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
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
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                 ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *         MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"         USING
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                     ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                           SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(医師情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYSYMD            TO  ORC-DBYMD
           MOVE    SYS-1010-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                 ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
          IF      MCP-RC           =   ZERO
                PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/08/10) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/10) end
           END-IF        
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(医師情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1010-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/08/10) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE  "DBCLOSE"                TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                 ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *****07/09/12  4.0対応  Satoh  START*****
      *         MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"         USING
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                     ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                           MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                           SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
      *
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
      *=== sarge対応 UP By Takashima(2006/08/10) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/10) end
      *****07/09/12  4.0対応  Satoh  START*****
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                 ORCMCPAREA
      *****07/09/12  4.0対応  Satoh  END*******
                                       MCPDATA-REC
      *****07/09/12  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/12  4.0対応  Satoh  END*******
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      *****07/09/12  4.0対応  Satoh  START*****
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *****07/09/12  4.0対応  Satoh  END*******
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
      *****07/09/12  4.0対応  Satoh  START*****
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *****07/09/12  4.0対応  Satoh  END*******
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
      *****07/09/12  4.0対応  Satoh  START*****
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *****07/09/12  4.0対応  Satoh  END*******
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      *****07/09/12  4.0対応  Satoh  START*****
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/12  4.0対応  Satoh  END*******
                                        MCPAREA
      *****07/09/12  4.0対応  Satoh  START*****
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *****07/09/12  4.0対応  Satoh  END*******
           .
       900-DBCLOSE-EXT.
           EXIT.
