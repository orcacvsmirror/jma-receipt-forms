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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A21000H03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 請求書兼領収書　印刷（ＨＣ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/10/03  出力先プリンタ名編集
      *  01.00.01    MCC-藤原     02/03/18  帳票ファイル名の変更
      *  01.00.02    MCC-多々納   02/05/01  マイナス表示を行うようにした
      *  01.00.03    NACL-多々納  02/09/13  自賠責・印刷サプの追加
      *  01.00.04    NACL-多々納  02/11/15  自費消費税対応修正
      *  01.00.05    NACL-多々納  03/03/04  複数診療科まとめ追加
      *  01.00.06    NACL-多々納  03/04/15  労災編集修正
      *  01.00.07    NACL-多々納  03/05/13  自賠責請求修正
      *  01.00.08    NACL-多々納  03/06/23  自費７桁対応
      *  01.00.09    NACL-多々納  03/06/30  出力先プリンタサブ追加
      *  01.00.10    NACL-太田    03/08/26  労災自賠責編集修正
      *  01.00.11    NACL-太田    03/09/12  前回未収額をマイナス表示可とする
      *  01.00.12    NACL-多々納  03/12/15  ＭＯＮＰＥ対応
      *                                     氏名３０文字対応
      *  01.00.13    NACL-太田    04/03/17  （再）の文字の編集を削除
      *  01.00.14    NACL-多々納  04/07/15  複数科・保険組合せ対応
      *  01.00.15    NACL-多々納  05/01/11  前回未収額の複数科・保険対応
      *  01.00.16    NACL-小豆沢  05/05/25  労災・自賠責の患者請求分は明細を
      *                                     表示するように変更
      *  01.00.17    NACL-多々納  06/04/21  MONFUNCへ変更
      *  01.00.18    NACL-多々納  06/07/07  領収書変更
      *  01.00.19    NACL-太田    06/08/22  パラメタテーブルより編集データ
      *                                     を参照する区分を追加
      *                                     月まとめ請求書発行機能追加
      *  03.03.00    NACL-多々納  06/09/27  返金対応追加
      *  03.04.01    NACL-森脇    07/01/17  医療機関名称、住所セット変更      
      *****************************************************************
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    @@@@-内田    07/03/18  再作成
      *  01.00.01    @@@@-佐藤    07/09/20  4.0対応
      *  01.00.02    @@@@-田中    08/02/08  再印刷処理の追加
      *                                     訂正時の合計印刷処理
      *  01.00.03    @@@@-高嶋    08/03/05 公務災害対応(ver.4.1)
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
      *****COPY    "HC03.INC".
      *   07/03/18  uchi
      *     COPY    "HCM03.INC"     REPLACING   //HCM03//
      *                             BY          //HC03//.
           COPY    "A21000H03.INC".
      *   07/03/18  uchi
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *    シェル用領域
      **** COPY    "CPCOMMONSHELL.INC".
      *
      *    シェル用領域
      *    COPY    "CPSHELLTBL.INC".
      *****COPY    "ORCA-DBPATH".
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *=== 公務災害対応 by k.takashima (2008/03/05) start
           03  FLG-PTHKNINF        PIC 9(01).
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      * 07/03/18 uchi
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
      * 07/03/18 uchi
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
      * 07/03/18 uchi
           03  IDX-JIHI            PIC 9(04).
           03  IDX7G.    
              05  IDX7             PIC 9(02).
      * 07/03/18 uchi
           03  IDX-SYU             PIC 9(04).
           03  IDH3                PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-HKNCOMBIMEI     PIC X(40).
      *
           03  WRK-SKYMONEY        PIC S9(11).
           03  WRK-MISYUMONEY      PIC S9(11).
           03  WRK-MISYU           PIC S9(11).
           03  WRK-GOKTEN          PIC S9(11).
      * 07/03/18 uchi
           03  WRK-SKYMONEY-TAX-SAI    PIC S9(11).
      * 07/03/18 uchi
      *
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
      *
           03  WRK-WARI            PIC 9(02)V9.
      *****03  WRK-Z6              PIC ZZZZZZ.
      *****03  WRK-Z62             PIC ZZZ9.9.
           03  WRK-Z6              PIC ZZZ.
           03  WRK-Z62             PIC 9.9.
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-NO              PIC Z(7).
      * 07/03/18 uchi
           03  WRK-Z6B-G.
      * 20080229 再印刷処理の追加START t.tanaka
      *         05  WRK-FIL             PIC X(07).
      * 20080229 再印刷処理の追加END
               05  WRK-Z6B             PIC ZZZ,ZZZ.
           03  WRK-Z7B-G.
      * 20080229 再印刷処理の追加START t.tanaka
      *         05  WRK-FIL             PIC X(09).
      * 20080229 再印刷処理の追加END
               05  WRK-Z7B             PIC Z,ZZZ,ZZZ.
      * 07/03/18 uchi
           03  WRK-Z7-G.
               05  WRK-FIL             PIC X(09).
               05  WRK-Z7              PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-G.
               05  WRK-FIL2            PIC X(18).
           03  WRK-Z72                 PIC S9(07).
           03  WRK-Z72-H               PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-HS              REDEFINES   WRK-Z72-H
                                       PIC -,---,---.
           03  WRK-Z72-H9              PIC Z,ZZZ,ZZ9.
      *    自費集計
           03  WRK-JIHI-REC.
               05  WRK-JIHI            PIC S9(07)   OCCURS  5.
           03  WRK-JIHI-TOTAL          PIC S9(08).
           03  WRK-JIHIMEI-REC.
               05  WRK-JIHIMEI         PIC X(20)   OCCURS  5.
      *
      *    複数科まとめ集計
           03  WRK-HKNTEN-KIN              PIC 9(07).
           03  WRK-HKNTEN-KEI              PIC 9(07)V99.
           03  WRK-NYUKIN                  PIC S9(07).
      *
           03  WRK-NYUKIN-TOTAL            PIC S9(07).
           03  WRK-ZENMISYU                PIC S9(07).
      *
           03  WRK-SRYKA-MEI               PIC X(30).
           03  WRK-PAGE                    PIC 9(03).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-LBLSRYYMD           PIC X(10).
           03  WRK-SRYYMDG             PIC X(50).
           03  WRK-STSRYYMDG           PIC X(22).
           03  WRK-EDSRYYMDG           PIC X(22).
      *
      *----(03.04.01)--ADD-START---
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL             PIC X(15).
      *****07/09/20  4.0対応  Satoh  START*****
           03  WRK-MSG                 PIC X(1000).
      *****07/09/20  4.0対応  Satoh  END*******
      *----(03.04.01)--ADD-END-----
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *----(03.04.01)--ADD-START---
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *----(03.04.01)--ADD-END-----
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
           COPY    "CPSYUPARA03.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *    請求書収納パラメタ
           COPY    "CPORCHC03SUB.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****07/09/20  4.0対応  Satoh  START*****
      *     COPY    "CPORCMCP.INC".
      *****07/09/20  4.0対応  Satoh  END*******
      *
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求書パラメタ
           COPY    "CPORCHC03.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC03AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
           IF    ( ORCHC03-PARAFLG     =   1 )
                   PERFORM 203-PARA-SYUNOU-SEC
           ELSE
               IF      ORCHC03-HAKHOUFLG   =   ZERO
      *            対象の伝票番号分のみ
                   PERFORM 201-ONLY-SYUNOU-SEC
               ELSE
      *            発行区分による
                   PERFORM 202-KAKHOU-SYUNOU-SEC
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    "HC03"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *    変更★★★★★★
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT              PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理 
      *****************************************************************
       400-PRINT-SEC                   SECTION.
      *
      *  07/03/18 uchi
            MOVE   HC03-TYPE1          TO  HC03-TYPE2
      *  07/03/18 uchi
      *　　印刷用データ出力
              WRITE  HC03
      *
      ******MOVE   "HC01.sh"           TO  SHELL-ID
      *     MOVE   "HC03.dia"          TO  SHELL-DIA
      *     MOVE   "HC03.def"          TO  SHELL-DEF
      *     MOVE   HC01PARA            TO  SHELL-DAT
      ***** MOVE   SYS-1031-PRTNM(3)   TO  SHELL-PRTNM 
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
      ***** MOVE   "HC03.dia"          TO  MKPRT-DIA
      ***** MOVE   "HC03.def"          TO  MKPRT-DEF
      * 07/03/18 uchi
      *      MOVE   "HCM03.red"         TO  MKPRT-DIA
            MOVE   "A21000H03.red"     TO  MKPRT-DIA
      * 07/03/18 uchi
            MOVE   SPACE               TO  MKPRT-DEF
            MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(3)   TO  MKPRT-PRTNM
      * 07/03/18 uchi
      *      CALL   "ORCSMKPRT"         USING
      * 20080229 再印刷処理の追加START t.tanaka
            MOVE  "請求書兼領収書"     TO MKPRT-INFO
      *      CALL   "ORCSMKPRTSITE"     USING
            CALL   "ORCSMKPRTSITE1"     USING
      * 20080229 再印刷処理の追加END
      * 07/03/18 uchi
                                       ORCSMKPRTAREA
      *****07/09/20  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/20  4.0対応  Satoh  END*******
      *
      *   印刷用スクリプト処理
      *★   ETC-EXEC SHELL   SHELL-AREA 10
      *    MOVE    SHELL-NAME          TO  SHELLTBL-NAME
      *    MOVE    SHELL-ARG1          TO  SHELLTBL-ARG1
      *    MOVE    SHELL-ARG2          TO  SHELLTBL-ARG2
      *    MOVE    SHELL-ARG3          TO  SHELLTBL-ARG3
      *    MOVE    SHELL-ARG4          TO  SHELLTBL-ARG4
      *    MOVE    SHELL-ARG5          TO  SHELLTBL-ARG5
      *    MOVE    SHELLTBL            TO  MCPDATA-REC
      *    MOVE    "SHELL"             TO  MCP-FUNC
      *    MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
      *    CALL    "MCPSUB"            USING
      *            MCPAREA
      **********   MCPDATA-REC
      *
      *
           .
       400-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理 (印刷ＤＢに出力）
      *****************************************************************
       401-PRINT-SEC                SECTION.
      *
           MOVE    HC03-TYPE1          TO  HC03-TYPE2
           MOVE    ORCHC03-PRTAREA     TO  ORCSPRTAREA
           MOVE    "A21000H03.red"     TO  SPRT-PRTID
           MOVE    HC03                TO  SPRT-PRTDATA
           MOVE    "2"                 TO  SPRT-SITEKBN
           DISPLAY "@@@ORCSPRTAREA = " ORCSPRTAREA
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
          IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "ORCSPRT ERR: PTID = " SPRT-PTID
           END-IF
      *
           .
       401-PRINT-EXT.
           EXIT.
      *****************************************************************
      *   対象の伝票番号分のみ処理
      *****************************************************************
       201-ONLY-SYUNOU-SEC                SECTION.
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
      *****07/09/20  4.0対応  Satoh  START*****
      *     MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    ORCHC03-DENPNUM     TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科名編集
           MOVE    SPACE                TO  WRK-SRYKA-MEI
           MOVE    SYU-SRYKA            TO  WRK-SRYKA
           PERFORM 1000-SRYKAMEI-HEN-SEC
      *    保険組合せ
           PERFORM 1001-HKNCOMBI-HEN-SEC
      *
           MOVE    ORCHC03-DENPNUM     TO  WRK-DENPNUM
           MOVE    ORCHC03-SEIKYU      TO  WRK-SKYMONEY
           MOVE    ORCHC03-NYUKIN      TO  WRK-NYUKIN
      **** MOVE    ORCHC03-ZENMISYU    TO  WRK-MISYUMONEY
      *---(03.03.00) LINE ADD  START -----------------------------------
           MOVE    ORCHC03-ZENMISYU-2  TO  WRK-MISYUMONEY
      *    前回請求額
           IF     (ORCHC03-ZEN-HENKIN  >   ZERO ) OR
                  (ORCHC03-KON-HENKIN (1)  >   ZERO )
      *        今回返金額
               COMPUTE WRK-MISYUMONEY      =   WRK-MISYUMONEY
                                           -   ORCHC03-ZEN-HENKIN
                                           -   ORCHC03-KON-HENKIN
                                                             (1)
           ELSE
      *        前回までの超過額
               COMPUTE WRK-MISYUMONEY      =   WRK-MISYUMONEY
                                           -   ORCHC03-KANYUKIN
               IF      WRK-MISYUMONEY      <   ZERO
                    MOVE    ZERO           TO  WRK-MISYUMONEY
               END-IF
           END-IF
      *    返金対応
           IF      (ORCHC03-HAKKOFLG   =   ZERO )  AND
                   (ORCHC03-NYUKIN     =   ZERO )  AND
                   (ORCHC03-ZEN-HENKIN >   ZERO )  AND
                   (WRK-MISYUMONEY     <   ZERO )  AND
                   ((WRK-MISYUMONEY * -1 )
                                       >   WRK-SKYMONEY)
      *        返金額が今回請求額より多い時、マイナス入金とする
               COMPUTE WRK-NYUKIN          =   WRK-SKYMONEY
                                           +   WRK-MISYUMONEY
           END-IF
      *---(03.03.00) LINE ADD  END   -----------------------------------
      *
      * 20080229 訂正時の合計印刷処理の追加START t.tanaka
           IF     (ORCHC03-TEIKIN-KBN  =   1   )  AND
                  (ORCHC03-HAKKOFLG    =   2   )
               MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
               MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN
           END-IF
      * 20080229 訂正時の合計印刷処理の追加END
      *
      *    帳票印刷開始処理
           PERFORM 400-SYOKI-PRINT-SEC
      *
      *    帳票編集処理
           PERFORM 300-HC03-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRINT-SEC
      *
           .
       201-ONLY-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
      *****07/09/20  4.0対応  Satoh  START*****
           INITIALIZE                  SYS-1001-REC
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SPACE               TO  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
      *    医療機関情報−所在地、連絡先
      *****07/09/20  4.0対応  Satoh  START*****
           INITIALIZE                  SYS-1002-REC
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SPACE               TO  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SPA-SRYYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1002-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
      *    労災医療機関情報編集
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SPA-SRYYMD          TO  SYS-4001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-4001-EDYUKYMD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-4001-REC
           ELSE
               INITIALIZE                  SYS-4001-REC
           END-IF
      *
      *    自費名称編集
           PERFORM 1002-JIHIMSG-HEN-SEC
      *
      *----(03.04.01)--ADD-START---
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *----(03.04.01)--ADD-END-----
      *
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       1002-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE     SPA-HOSPNUM        TO SYS-1013-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1013-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1013-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           INITIALIZE                  WRK-JIHIMEI-REC
           PERFORM
               UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1013-REC
               IF      SPA-NYUGAIKBN       =   SYS-1013-KBNCD(1:1)
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (5)
                   END-EVALUATE
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           .
       1002-JIHIMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       1001-HKNCOMBI-HEN-SEC             SECTION.
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
      *****07/09/20  4.0対応  Satoh  START*****
      *     MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-HOSPNUM          TO  COMB-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                DISPLAY "*** HKNCOMBI  ERR:"   COMB-KEY "."
           END-IF
           PERFORM 10011-HKNCOMBIMEI-HEN-SEC
      *
      *  07/03/18 uchi
      *    患者保険情報検索
           PERFORM 900-PTHKNINF-READ-SEC
      *  07/03/18 uchi
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *
      *        患者労災保険情報検索
               INITIALIZE                      PTRSIINF-REC
      *****07/09/20  4.0対応  Satoh  START*****
      *         MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    SPA-HOSPNUM          TO  PTRSI-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               PERFORM 910-PTRSIINF-READ-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *=== 公務災害対応 by k.takashima (2008/03/05) start
           PERFORM 10011-HKNCOMBIMEI-HEN-SEC
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *
           .
       1001-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       10011-HKNCOMBIMEI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *     STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
      *             "　"                    DELIMITED  BY  SIZE
      *             COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
      *             "　"                    DELIMITED  BY  SIZE
      *             COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
      *             "　"                    DELIMITED  BY  SIZE
      *             COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
      *             "　"                    DELIMITED  BY  SIZE
      *             COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
      *                   INTO              WRK-HKNCOMBIMEI
      *     END-STRING
           IF  ( COMB-HKNNUM   =  "971" )
           AND ( PTRSI-HKNKBN  =  "5"   )
               MOVE    PTHKN-KIGO     TO   WRK-HKNCOMBIMEI
           ELSE
               STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                             INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *
           .
       10011-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       1000-SRYKAMEI-HEN-SEC                SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE     SPA-HOSPNUM        TO  SYS-1005-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC 
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               IF      WRK-SRYKA-MEI       =   SPACE
                   MOVE    SYS-1005-SRYKANAME2
                                               TO  WRK-SRYKA-MEI
               ELSE
                   IF      WRK-SRYKA-MEI(19:2) NOT =   SPACE
                       CONTINUE
                   ELSE
                       STRING  WRK-SRYKA-MEI   DELIMITED  BY  SPACE
                               "、"            DELIMITED  BY  SIZE
                               SYS-1005-SRYKANAME2
                                               DELIMITED  BY  SPACE
                               INTO                WRK-SRYKA-MEI
                       END-STRING
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           .
       1000-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HC03-HEN-SEC                SECTION.
      *
           INITIALIZE                  HC03
      *
      *    伝票番号
           MOVE    WRK-DENPNUM         TO  WRK-NO  
           MOVE    WRK-NO              TO  HC03-NO
      *    診療科
      *     MOVE    WRK-SRYKA-MEI(1:20) TO  HC03-MIDASI
      *    患者番号
           MOVE    SPA-PTNUM           TO  HC03-KNJNO
      * 07/03/18 uchi
      *    保険者番号
           MOVE    PTHKN-HKNJANUM      TO  HC03-HKNJANO
      * 07/03/18 uchi
      *    患者名称
           MOVE    SPA-NAME            TO  HC03-KNJNAMW
      * 07/03/18 uchi
      *    保険種別
      *     MOVE    WRK-HKNCOMBIMEI     TO  HC03-HOKEN
      * 07/03/18 uchi
      *    負担割合
           IF      SYU-PTFTNRATE       >   ZERO
               COMPUTE WRK-WARI        =   SYU-PTFTNRATE   /   10
               IF      SYU-PTFTNRATE(3:1)  =   ZERO
                   MOVE    WRK-WARI            TO  WRK-Z6
                   MOVE    WRK-Z6              TO  HC03-FUTAN
               ELSE
                   MOVE    WRK-WARI            TO  WRK-Z62
                   MOVE    WRK-Z62             TO  HC03-FUTAN
               END-IF
           END-IF
      * 07/03/18 uchi
      *    診療日
      *     PERFORM 3101-SRYYMD-HEN-SEC
      *     MOVE    WRK-LBLSRYYMD       TO  HC03-LBLSRYYMD
      *     MOVE    WRK-SRYYMDG         TO  HC03-SRYYMD
      *    発行日
      *     MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
      *     PERFORM 31012-SEIWA-HEN-SEC
      *     MOVE    WRK-HENYMDG         TO  HC03-HAKYMD
            MOVE      SYU-DENPPRTYMD(1:4) TO  HC03-HAKYYYY
            MOVE      SYU-DENPPRTYMD(5:2) TO  HC03-HAKMM
            MOVE      SYU-DENPPRTYMD(7:2) TO  HC03-HAKDD
      * 07/03/18 uchi
      *
      *----(03.04.01)--UPD-START---
      *    医療機関住所
      *    MOVE    SYS-1002-ADRS       TO  HC03-IRYOADRS
      *    医療機関名
      *    MOVE    SYS-1001-HOSPNAME   TO  HC03-IRYOMEI
      *    医療機関ＴＥＬ
      **** MOVE    SYS-1002-TEL        TO  HC03-IRYOTEL
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HC03-IRYOADRS
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HC03-IRYOMEI
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HC03-IRYOTEL
      *----(03.04.01)--UPD-END-----
      *
      *    自費名称編集
      *     PERFORM 3102-JIHIMSG-HEN-SEC
      * 07/03/18 uchi
           INITIALIZE                      WRK-GOKTEN
           IF SYU-HKNTEN (01)     >      ZERO
              PERFORM   310-SRHANTEI-SEC
           END-IF
      * 07/03/18 uchi
           MOVE    SPACE               TO  WRK-Z7-G
           MOVE    SPACE               TO  WRK-Z72-G
      * 07/03/18 uchi
      *    診療科
      *     MOVE    SYU-HKNTEN (01)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO1
      *    指導科
      *     MOVE    SYU-HKNTEN (02)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO2
      *    在宅科
      *     MOVE    SYU-HKNTEN (03)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO3
      *    投薬科
      *     MOVE    SYU-HKNTEN (04)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO4
      *    注射科
      *     MOVE    SYU-HKNTEN (05)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO5
      *    処置科
      *     MOVE    SYU-HKNTEN (06)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO6
      *    手術科
      *     MOVE    SYU-HKNTEN (07)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO7
      *    麻酔
      *     MOVE    SYU-HKNTEN (08)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO8
      *    検査科
      *     MOVE    SYU-HKNTEN (09)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO9
      *    Ｘ線科
      *     MOVE    SYU-HKNTEN (10)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G          TO  HC03-HKNRYO10
      *    リハビリ
      *     MOVE    SYU-HKNTEN (11)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO11
      *    精神科専門
      *     MOVE    SYU-HKNTEN (12)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO12
      *    放射線治療
      *     MOVE    SYU-HKNTEN (13)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO13
      *    入院料
      *     MOVE    SYU-HKNTEN (14)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO14
      *    療養担当手当て
      *     MOVE    SYU-HKNTEN (15)     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HKNRYO15
      *    見出し
      *     PERFORM 3001-MEISAI-HEN-SEC
      *    初診・再診
           MOVE    SYU-HKNTEN (01)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO1
      *    医学管理
           MOVE    SYU-HKNTEN (02)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO2
      *    在宅科
           MOVE    SYU-HKNTEN (03)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO3
      *    投薬科
           MOVE    SYU-HKNTEN (04)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO4
      *    注射科
           MOVE    SYU-HKNTEN (05)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO5
      *    処置科
           MOVE    SYU-HKNTEN (06)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO6
      *    手術科
           MOVE    SYU-HKNTEN (07)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO7
      *    麻酔科
           MOVE    SYU-HKNTEN (08)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO8
      *    検査科
           MOVE    SYU-HKNTEN (09)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO9
      *    X線科
           MOVE    SYU-HKNTEN (10)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO10
      *    リハ科
           MOVE    SYU-HKNTEN (11)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO11
      *    精神科
           MOVE    SYU-HKNTEN (12)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO12
      *    放射線科
           MOVE    SYU-HKNTEN (13)     TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-HKNRYO13
      * 07/03/18 uchi
      *    合計点数
           COMPUTE WRK-GOKTEN          =   SYU-HKNTEN (01)
                                       +   SYU-HKNTEN (02)
                                       +   SYU-HKNTEN (03)
                                       +   SYU-HKNTEN (04)
                                       +   SYU-HKNTEN (05)
                                       +   SYU-HKNTEN (06)
                                       +   SYU-HKNTEN (07)
                                       +   SYU-HKNTEN (08)
                                       +   SYU-HKNTEN (09)
                                       +   SYU-HKNTEN (10)
                                       +   SYU-HKNTEN (11)
                                       +   SYU-HKNTEN (12)
                                       +   SYU-HKNTEN (13)
                                       +   SYU-HKNTEN (14)
                                       +   SYU-HKNTEN (15)
      * 07/03/18 uchi
      *     MOVE    WRK-GOKTEN          TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-GOKTEN
           MOVE    WRK-GOKTEN          TO  WRK-Z6B
           MOVE    WRK-Z6B-G           TO  HC03-GOKTEN
      * 07/03/18 uchi
      *
      *    労災・自賠で患者請求が無い場合、点数表示なし
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *     IF    ( COMB-HKNNUM             =    "971"  AND
      *             SYS-4001-RSIIRYOKBN     =    "1" )  OR
      *           ( COMB-HKNNUM             =    "973"  AND
      *             SYU-PTFTNRATE           =    ZERO )
           IF    ( COMB-HKNNUM             =    "971"
           AND     PTRSI-HKNKBN        NOT =    "5"
           AND     SYS-4001-RSIIRYOKBN     =    "1" )
           OR    ( COMB-HKNNUM             =    "971"
           AND     PTRSI-HKNKBN            =    "5"
           AND     SYU-PTFTNRATE           =    ZERO )
           OR    ( COMB-HKNNUM             =    "973"
           AND     SYU-PTFTNRATE           =    ZERO )
      *=== 公務災害対応 by k.takashima (2008/03/05) end
               MOVE    SPACE               TO  HC03-HKNRYO1
               MOVE    SPACE               TO  HC03-HKNRYO2
               MOVE    SPACE               TO  HC03-HKNRYO3
               MOVE    SPACE               TO  HC03-HKNRYO4
               MOVE    SPACE               TO  HC03-HKNRYO5
               MOVE    SPACE               TO  HC03-HKNRYO6
               MOVE    SPACE               TO  HC03-HKNRYO7
               MOVE    SPACE               TO  HC03-HKNRYO8
               MOVE    SPACE               TO  HC03-HKNRYO9
               MOVE    SPACE               TO  HC03-HKNRYO10
               MOVE    SPACE               TO  HC03-HKNRYO11
               MOVE    SPACE               TO  HC03-HKNRYO12
               MOVE    SPACE               TO  HC03-HKNRYO13
      * 07/03/18 uchi
      *         MOVE    SPACE               TO  HC03-HKNRYO14
      *         MOVE    SPACE               TO  HC03-HKNRYO15
      * 07/03/18 uchi
               MOVE    ALL "*"             TO  HC03-GOKTEN
           END-IF
      *
      * 07/03/18 uchi
      *    保険適用分
      *     MOVE    SYU-HKNTEKMONEY     TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HOKENBUN
      *
      *     IF    ( COMB-HKNNUM             =    "971"
      *                                     OR   "973" )
      *        労災・自賠の時、点数表示なし
      *         EVALUATE    TRUE
      *         WHEN      ( COMB-HKNNUM             =    "971" )
      *             AND   ( SYS-4001-RSIIRYOKBN NOT =   "1" )
      *
      *         WHEN      ( COMB-HKNNUM             =    "973" )
      *             AND   ( PTRSI-HKNKBN        =    "4" ) 
      *             AND   ( SYU-PTFTNRATE   NOT =   ZERO  )
      *
      *             COMPUTE WRK-GOKTEN      =    SYU-HKNTEKMONEY
      *                                     +    SYU-RSISHOSHIN-MONEY
      *                                     +    SYU-RSISAISHIN-MONEY
      *                                     +    SYU-RSISDO-MONEY
      *                                     +    SYU-RSIETC-MONEY
      *             MOVE    WRK-GOKTEN      TO  WRK-Z72
      *             PERFORM 800-HENKAN-SEC
      *             MOVE    WRK-Z72-G           TO  HC03-HOKENBUN
      *         WHEN    OTHER
      *             MOVE    SPACE               TO  HC03-HOKENBUN
      *         END-EVALUATE
      *     END-IF
      *
      *    労災・自賠の診察料等の記載
      *     IF   (SYU-RSI-TOTAL     NOT =   ZERO) AND
      *          (SYS-4001-RSIPRTKBN    =   "2")
      *         EVALUATE    COMB-HKNNUM
      *             WHEN    "971"
      *                 MOVE  "（内労災診察等）"  TO HC03-RSIMIDASI
      *             WHEN    "973"
      *                 MOVE  "（内自賠診察等）"  TO HC03-RSIMIDASI
      *         END-EVALUATE
      *         MOVE    SYU-RSI-TOTAL          TO  WRK-Z72
      *         PERFORM 800-HENKAN-SEC
      *         MOVE    WRK-Z72-G              TO  HC03-RSIBUN
      *         MOVE    "円"                   TO  HC03-RSIEN
      *     ELSE
      *         MOVE    SPACE                  TO  HC03-RSIMIDASI
      *         MOVE    SPACE                  TO  HC03-RSIBUN
      *         MOVE    SPACE                  TO  HC03-RSIEN
      *     END-IF
      * 07/03/18 uchi
      *
      *    保険適用外分
           MOVE    ZERO                TO  WRK-GOKTEN
      *=== 病理診断対応 by k.takashima (2008/03/05) start
      *     COMPUTE WRK-GOKTEN          =   SYU-TGMONEY (16)
      *                                 +   SYU-TGMONEY-TAX  (16)
           COMPUTE WRK-GOKTEN          =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
      *=== 病理診断対応 by k.takashima (2008/03/05) end
      * 07/03/18 uchi
      *     MOVE    WRK-GOKTEN          TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-HOKENGAI
      * 07/03/18 uchi
      *    自費１
           MOVE    ZERO                TO  WRK-JIHI-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
      * 07/03/18 uchi
      *         COMPUTE WRK-JIHI (IDX)      =   SYU-JIHI-TAXNASI (IDX)
      *                                     +   SYU-JIHI-TAXARI  (IDX)
               ADD   SYU-JIHI-TAXNASI (IDX)   TO   WRK-GOKTEN
               ADD   SYU-JIHI-TAXARI  (IDX)   TO   WRK-GOKTEN
      * 07/03/18 uchi
           END-PERFORM
      * 07/03/18 uchi
           ADD   SYU-JIHI-TAX          TO  WRK-GOKTEN
           MOVE    WRK-GOKTEN          TO  WRK-Z7B
           MOVE    WRK-Z7B-G           TO  HC03-HOKENGAI
      *
      *     MOVE    WRK-JIHI (1)        TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-JIHI1
      *     MOVE    WRK-JIHI (2)        TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-JIHI2
      *     MOVE    WRK-JIHI (3)        TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-JIHI3
      *     MOVE    WRK-JIHI (4)        TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-JIHI4
      *     MOVE    WRK-JIHI (5)        TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-JIHI5
      *    自費計
      *     COMPUTE WRK-JIHI-TOTAL      =   SYU-JIHI-TOTAL
      *                                 +   SYU-JIHI-TOTAL-TAX
      *     MOVE    WRK-JIHI-TOTAL      TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-JIHI6
      *    自費消費税
      *     MOVE    SYU-JIHI-TAX        TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *    MOVE    WRK-Z72-G           TO  HC03-JIHI7
      *    自費合計
      *     IF      SYU-SRYYMD          <=  "20070331"
      *       COMPUTE WRK-GOKTEN        =   WRK-JIHI-TOTAL
      *                                 +   SYU-JIHI-TAX
      *     ELSE
      *       MOVE    WRK-JIHI-TOTAL    TO  WRK-GOKTEN
      *     END-IF
      *     MOVE    WRK-GOKTEN          TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-JIHI8
      *    薬剤一部負担
      *     MOVE    SYU-YKZ-FTN         TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-YAKUZAI
      *    老人一部負担金
      *     MOVE    SYU-RJN-FTN         TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-ROUJIN
      *    公費一部負担金
      *     MOVE    SYU-KOH-FTN         TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-KOUHI
           INITIALIZE                         WRK-GOKTEN
           ADD     SYU-YKZ-FTN            TO  WRK-GOKTEN
      *    老人一部負担金
           ADD     SYU-RJN-FTN            TO  WRK-GOKTEN
      *    公費一部負担金
           ADD     SYU-KOH-FTN            TO  WRK-GOKTEN
           MOVE    WRK-GOKTEN             TO  WRK-Z7
           MOVE    WRK-Z7-G               TO  HC03-FUTANKIN
      *
      *    調整金
      *     MOVE    SYU-CHOSEI          TO  WRK-Z72
      *     PERFORM 800-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-CHOSEI
      *    今回請求額
      *     MOVE    WRK-SKYMONEY        TO  WRK-Z72
      *     PERFORM 810-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-KONSEIKYU
           MOVE    WRK-SKYMONEY        TO  WRK-Z7B
           MOVE    WRK-Z7B-G           TO  HC03-KONSEIKYU
      *
      *    今回入金額
      *     MOVE    WRK-NYUKIN          TO  WRK-Z72
      *     PERFORM 810-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-KONNYUKIN
           MOVE    WRK-NYUKIN          TO  WRK-Z7
           MOVE    WRK-Z7-G            TO  HC03-KONNYUKIN
      * 07/03/18 uchi
      *    前回未収額計算
      *!!! PERFORM 3101-MISYU-HEN-SEC
      *****MOVE    ORCHC03-ZENMISYU    TO  WRK-MISYUMONEY
      *     MOVE    WRK-MISYUMONEY      TO  WRK-Z72
      *     PERFORM 810-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-ZENMISYU
           MOVE    WRK-MISYUMONEY      TO  WRK-Z7B
           MOVE    WRK-Z7B-G           TO  HC03-ZENMISYU
      * 07/03/18 uchi
      *    合計請求額
      *     COMPUTE WRK-GOKTEN          =   WRK-SKYMONEY
      *                                 +   WRK-MISYUMONEY
      *     MOVE    WRK-GOKTEN          TO  WRK-Z72
      *     PERFORM 810-HENKAN-SEC
      *     MOVE    WRK-Z72-G           TO  HC03-GOUKEIGAK
      *
      *    今回未収金
           COMPUTE WRK-GOKTEN          =   WRK-SKYMONEY
                                       -   WRK-NYUKIN
           IF   WRK-GOKTEN             >   0 
                MOVE    WRK-GOKTEN          TO  WRK-Z7B
                MOVE    WRK-Z7B-G           TO  HC03-KONMISYU
           END-IF
      *    発行区分
      *     EVALUATE    ORCHC03-HAKKOFLG
      *         WHEN    1
      *            MOVE    "（再）"        TO  HC03-MIDASHI
      *         WHEN    2
      *            MOVE    "（訂正）"      TO  HC03-MIDASHI
      *     END-EVALUATE
      * 07/03/18 uchi
      *
           .
       300-HC03-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   明細見出し編集処理
      *****************************************************************
      * 3001-MEISAI-HEN-SEC                SECTION.
      *
      *     MOVE    "初 ・ 再 診 料"
      *                                 TO  HC03-MEISAI (1)
      *     MOVE    "医 学 管 理 等"
      *                                 TO  HC03-MEISAI (2)
      *     MOVE    "在　宅　医　療"
      *                                 TO  HC03-MEISAI (3)
      *     MOVE    "投　　　　　薬"
      *                                 TO  HC03-MEISAI (4)
      *     MOVE    "注　　　　　射"
      *                                 TO  HC03-MEISAI (5)
      *     MOVE    "処　　　　　置"
      *                                 TO  HC03-MEISAI (6)
      *     MOVE    "手　　　　　術"
      *                                 TO  HC03-MEISAI (7)
      *     MOVE    "麻　　　　　酔"
      *                                 TO  HC03-MEISAI (8)
      *     MOVE    "検　　　　　査"
      *                                 TO  HC03-MEISAI (9)
      *     MOVE    "画　像　診　断"
      *                                 TO  HC03-MEISAI (10)
      *     MOVE    "リハビリテーション"
      *                                 TO  HC03-MEISAI (11)
      *     MOVE    "精神科専門療法"
      *                                 TO  HC03-MEISAI (12)
      *     MOVE    "放 射 線 治 療"
      *                                 TO  HC03-MEISAI (13)
      *     MOVE    "入　院　料　等"
      *                                 TO  HC03-MEISAI (14)
      *     MOVE    "療養担当手当"
      *                                 TO  HC03-MEISAI (15)
      *    明細削除
      *    療養担当手当て
      *     IF      SYU-HKNTEN (15)     =   ZERO
      *         MOVE    SPACE               TO  HC03-MEISAI (15)
      *     END-IF
      *
           .
       3001-MEISAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *   複数科・保険入力の印刷処理
      *****************************************************************
       202-KAKHOU-SYUNOU-SEC                SECTION.
      *
           INITIALIZE                  ORCHC03SUBAREA
      *    収納マスタまとめ計算
           CALL    "ORCHC03SUB"        USING
                                       SPA-AREA
                                       ORCHC03AREA
                                       ORCHC03SUBAREA
      *    入金合計
           MOVE    ZERO                TO  WRK-NYUKIN-TOTAL
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
               ADD     ORCHC03SUB-NYUKIN-TOTAL(IDX-SYU)
                                           TO  WRK-NYUKIN-TOTAL
           END-PERFORM
      *    入金合計から前回未収額の入金額を求める
           IF      ORCHC03-NYUKIN      >   WRK-NYUKIN-TOTAL
               COMPUTE WRK-ZENMISYU    =   ORCHC03-NYUKIN
                                       -   WRK-NYUKIN-TOTAL
           ELSE
               MOVE    ZERO            TO  WRK-ZENMISYU
           END-IF
      *
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
               MOVE    ORCHC03SUB-SYUNOU-REC(IDX-SYU)
                                           TO  SYUNOU-REC
      *        診療科名編集
               MOVE    SPACE               TO  WRK-SRYKA-MEI
               IF      ORCHC03SUB-DOUJI-SRYKA-G (IDX-SYU)
                                           =   SPACE
                   MOVE    SYU-SRYKA           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               ELSE
                   PERFORM 2021-GRP-SRYKAMEI-HEN-SEC
               END-IF
      *        保険組合せ
               IF      SYU-HKNCOMBINUM     =   ZERO  OR  SPACE
                   MOVE    "複数保険使用"      TO  WRK-HKNCOMBIMEI
                   MOVE    ZERO                TO  SYU-PTFTNRATE
               ELSE
                   PERFORM 1001-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
               MOVE    ORCHC03SUB-SKYMONEY (IDX-SYU)
                                           TO  WRK-SKYMONEY
               MOVE    ORCHC03SUB-NYUKIN-TOTAL(IDX-SYU)
                                           TO  WRK-NYUKIN
      *
      *        １枚目に前回未収額を表示する
               IF      IDX-SYU             =   1
      *---(03.03.00) LINE ADD  START -----------------------------------
                  AND  (ORCHC03-HAKKOFLG   =   ZERO )
      ***********  MOVE    ORCHC03-ZENMISYU    TO  WRK-MISYUMONEY
                   MOVE    ORCHC03-ZENMISYU-2  TO  WRK-MISYUMONEY
      *---(03.03.00) LINE ADD  END   -----------------------------------
                   COMPUTE WRK-NYUKIN          =   WRK-NYUKIN
                                               +   WRK-ZENMISYU
               ELSE
                   MOVE    ZERO                TO  WRK-MISYUMONEY
               END-IF
      *---(03.03.00) LINE ADD  START -----------------------------------
               IF      IDX-SYU             =   1
                   IF     (ORCHC03-ZEN-HENKIN  >   ZERO )  OR
                          (ORCHC03SUB-HENKIN (IDX-SYU) >  ZERO)
                       COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                               -   ORCHC03-ZEN-HENKIN
                                               -   ORCHC03SUB-HENKIN
                                                         (IDX-SYU)
                   ELSE
                       COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                               -   ORCHC03-KANYUKIN
                       IF      WRK-MISYUMONEY      <   ZERO
                           MOVE    ZERO            TO  WRK-MISYUMONEY
                       END-IF
                   END-IF
      *            返金対応
                   IF      (ORCHC03-HAKKOFLG   =   ZERO )  AND
                           (WRK-NYUKIN         =   ZERO )  AND
                           (ORCHC03-ZEN-HENKIN >   ZERO )  AND
                           (WRK-MISYUMONEY     <   ZERO )  AND
                           ((WRK-MISYUMONEY * -1)
                                                >   WRK-SKYMONEY)
      *             返金額が今回請求額より多い時、マイナス入金とする
                       COMPUTE WRK-NYUKIN      =   WRK-SKYMONEY
                                               +   WRK-MISYUMONEY
                   END-IF
               ELSE
                   COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                           -   ORCHC03SUB-HENKIN
                                                         (IDX-SYU)
               END-IF
      *---(03.03.00) LINE ADD  END   -----------------------------------
      * 20080229 訂正時の合計印刷処理の追加START t.tanaka
           IF     (ORCHC03-TEIKIN-KBN  =   1   )  AND
                  (ORCHC03-HAKKOFLG    =   2   )
               MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
               MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN
           END-IF
      * 20080229 訂正時の合計印刷処理の追加END
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HC03-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
      *
           .
       202-KAKHOU-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *   パラメタテーブルからの印刷処理
      *****************************************************************
       203-PARA-SYUNOU-SEC                 SECTION.
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
               MOVE    SYUPARA03-SYU-DATA  TO  SYUNOU-REC
      *
               MOVE    SYUPARA03-DENPNUM   TO  WRK-DENPNUM
               MOVE    SYUPARA03-SEIKYU    TO  WRK-SKYMONEY
               MOVE    SYUPARA03-NYUKIN    TO  WRK-NYUKIN
               MOVE    SYUPARA03-MISYU     TO  WRK-MISYUMONEY
               MOVE    SYUPARA03-SRYKAMEI  TO  WRK-SRYKA-MEI
      *
               INITIALIZE                      HKNCOMBI-REC
                                               PTRSIINF-REC
               IF    ( SYUPARA03-HKNCOMBI-MAX  =   1 )
                   PERFORM 1001-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    SYUPARA03-HKNCOMBIMEI
                                           TO  WRK-HKNCOMBIMEI
      *
               IF    ( ORCHC03-PRTKBN      =   "1" )
      *
      *            帳票編集処理
                   PERFORM 300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 401-PRINT-SEC
               ELSE
      *
      *            帳票印刷開始処理
                   PERFORM 400-SYOKI-PRINT-SEC
      *
      *            帳票編集処理
                   PERFORM 300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 400-PRINT-SEC
               END-IF
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       203-PARA-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    複数診療科編集処理
      *****************************************************************
       2021-GRP-SRYKAMEI-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL       IDH3    >   10
               IF      ORCHC03SUB-DOUJI-SRYKA (IDX-SYU IDH3)
                                       NOT =   SPACE
                   MOVE    ORCHC03SUB-DOUJI-SRYKA(IDX-SYU IDH3)
                                           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               END-IF
           END-PERFORM
           .
       2021-GRP-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診／再診判定処理
      *****************************************************************
       310-SRHANTEI-SEC      SECTION.
      *
      ***  受診履歴情報読み込み
           PERFORM 900-JYURRK-READ-SEC
           MOVE JYURRK-SRYYMD(7:2) TO  IDX7G
      ***  診療会計情報読み込み
           PERFORM 900-SRYACCT-READ-SEC
           IF    FLG-SRYACCT    =  ZERO
            AND  ACCT-DAY(1 IDX7)  >  ZERO
            AND  JYURRK-RENNUM  =  1
      *  初診料を印字する。
                 MOVE  SYU-HKNTEN (01)  TO  WRK-Z6B
                 MOVE  WRK-Z6B-G        TO  HC03-HKNRYO1
           ELSE 
      *  再診料を印字する。
                 MOVE  SYU-HKNTEN (01)  TO  WRK-Z6B
                 MOVE  WRK-Z6B-G        TO  HC03-HKNRYO1
           END-IF
           .           
       310-SRHANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理
      *****************************************************************
       800-HENKAN-SEC             SECTION.
     *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
      *****MOVE    WRK-Z72             TO  KANACHK-MAE-INPUT
           IF      WRK-Z72             <   ZERO
               MOVE    WRK-Z72             TO  WRK-Z72-HS
               MOVE    WRK-Z72-HS          TO  KANACHK-MAE-INPUT
           ELSE
               MOVE    WRK-Z72             TO  WRK-Z72-H
               MOVE    WRK-Z72-H           TO  KANACHK-MAE-INPUT
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
           END-IF
           .
       800-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理（ゼロ表示用）
      *****************************************************************
       810-HENKAN-SEC             SECTION.
     *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           IF      WRK-Z72             <   ZERO
               MOVE    WRK-Z72             TO  WRK-Z72-HS
               MOVE    WRK-Z72-HS          TO  KANACHK-MAE-INPUT
           ELSE
               MOVE    WRK-Z72             TO  WRK-Z72-H9
               MOVE    WRK-Z72-H9           TO  KANACHK-MAE-INPUT
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
           END-IF
           .
       810-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金額編集処理
      *****************************************************************
      *3101-MISYU-HEN-SEC             SECTION.
      *
      *    MOVE    ZERO                TO  WRK-MISYUMONEY
      *    MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
      *    収納テーブル読み込み
      *    INITIALIZE                  SYUNOU-REC
      *    MOVE    SPA-HOSPID          TO  SYU-HOSPID
      *    MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
      *    MOVE    SPA-PTID            TO  SYU-PTID
      *
      *    MOVE    SYUNOU-REC          TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      *        PERFORM 980-SYUNOU-READ-SEC
      *    ELSE
      *        INITIALIZE                  SYUNOU-REC
      *        MOVE    1               TO  FLG-SYUNOU
      *    END-IF
      *    MOVE    ZERO                TO  WRK-MISYUMONEY
      *    PERFORM
      *        UNTIL     FLG-SYUNOU    =   1
      *        IF      ORCHC03-FUKU-DENPNUM    =   ZERO
      *            IF      SYU-DENPNUM     NOT =   WRK-DENPNUM
      *                COMPUTE WRK-MISYU       =   SYU-SKYMONEY
      *                                        -   SYU-NYUKIN-TOTAL
      *                ADD     WRK-MISYU           TO  WRK-MISYUMONEY
      *            END-IF
      *        ELSE
      *            IF      SYU-FUKU-DENPNUM    =   ORCHC03-FUKU-DENPNUM
      *                CONTINUE
      *            ELSE
      *                COMPUTE WRK-MISYU       =   SYU-SKYMONEY
      *                                        -   SYU-NYUKIN-TOTAL
      *                ADD     WRK-MISYU           TO  WRK-MISYUMONEY
      *            END-IF
      *        END-IF
      *
      *        MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      *        PERFORM 980-SYUNOU-READ-SEC
      *    END-PERFORM
      *
      *    MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *    MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    .
      *3101-MISYU-HEN-EXT.
      *****EXIT.
      *
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
      * 3102-JIHIMSG-HEN-SEC             SECTION.
      *
      *     MOVE    WRK-JIHIMEI(1)      TO  HC03-JIHIMSG1
      *     MOVE    WRK-JIHIMEI(2)      TO  HC03-JIHIMSG2
      *     MOVE    WRK-JIHIMEI(3)      TO  HC03-JIHIMSG3
      *     MOVE    WRK-JIHIMEI(4)      TO  HC03-JIHIMSG4
      *     MOVE    WRK-JIHIMEI(5)      TO  HC03-JIHIMSG5
      *
      *     .
      * 3102-JIHIMSG-HEN-EXT.
      *     EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       3101-SRYYMD-HEN-SEC         SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYYMDG
                                               WRK-LBLSRYYMD
      *
           IF    ( ORCHC03-HAKKOFLG    =   4 )
                   MOVE    "診療年月"      TO  WRK-LBLSRYYMD
                   MOVE    SYU-SRYYMD      TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG  (1:16)
                                           TO  WRK-SRYYMDG
           ELSE
               IF    ( ORCHC03-PARAFLG         =   1 )
                AND  ( SYUPARA03-STSRYYMD  NOT =   SYUPARA03-EDSRYYMD )
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   PERFORM 31012-SRYYMD-HEN-SEC
               ELSE
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   MOVE    SYU-SRYYMD      TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  WRK-SRYYMDG
               END-IF
           END-IF
      *
           .
       3101-SRYYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       31012-SRYYMD-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYYMDG
      *
           MOVE    SYUPARA03-STSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-STSRYYMDG
      *
           MOVE    SYUPARA03-EDSRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-EDSRYYMDG
      *
           STRING  WRK-STSRYYMDG       DELIMITED   BY  SIZE
                   " 〜 "              DELIMITED   BY  SIZE
                   WRK-EDSRYYMDG       DELIMITED   BY  SIZE
           INTO    WRK-SRYYMDG
           END-STRING
      *
           .
       31012-SRYYMD-HEN-EXT.
           EXIT.
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
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-START---
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(10) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM            TO  PARA-HOSPNUM
      *****07/09/20  4.0対応  Satoh  END*******
           MOVE    ORCHC03-PARA-GYOUMUID  TO  PARA-GYOUMUID
           MOVE    ORCHC03-PARA-TERMID    TO  PARA-TERMID
           MOVE    ORCHC03-PARA-FILEMEI   TO  PARA-FILEMEI
           MOVE    PARA-REC               TO  MCPDATA-REC
           MOVE    "tbl_para"             TO  MCP-TABLE
           MOVE    "key5"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-PARA-KEY5-FET-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-PARA
      *
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC       SECTION.
      *
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
     *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       910-PTRSIINF-READ-SEC       SECTION.
      *
           MOVE    PTRSIINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   MOVE    ZERO            TO  FLG-PTRSIINF
               ELSE
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
     *
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報検索 20030613追加
      *****************************************************************
       900-PTHKNINF-READ-SEC                 SECTION.
      *
           INITIALIZE                    PTHKNINF-REC
      *
      *****07/09/20  4.0対応  Satoh  START*****
      *     MOVE    SYU-HOSPID       TO   PTHKN-HOSPID
           MOVE    SYU-HOSPNUM      TO   PTHKN-HOSPNUM
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SYU-PTID         TO   PTHKN-PTID
           MOVE    COMB-HKNID       TO   PTHKN-HKNID
      *
           MOVE    PTHKNINF-REC     TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"   TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  PTHKNINF-REC
                   MOVE    ZERO          TO  FLG-PTHKNINF
               ELSE
                   MOVE    1             TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報検索
      *****************************************************************
       900-JYURRK-READ-SEC                 SECTION.
      *
           INITIALIZE                  JYURRK-REC
      *
      *****07/09/20  4.0対応  Satoh  START*****
      *     MOVE    SYU-HOSPID       TO   JYURRK-HOSPID
           MOVE    SYU-HOSPNUM      TO   JYURRK-HOSPNUM
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE    SYU-PTID         TO   JYURRK-PTID
           MOVE    SYU-NYUGAIKBN    TO   JYURRK-NYUGAIKBN
           MOVE    WRK-DENPNUM      TO   JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"     TO  MCP-TABLE
           MOVE    "key6"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"     TO  MCP-TABLE
               MOVE    "key6"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  JYURRK-REC
                   MOVE    ZERO          TO  FLG-JYURRK
               ELSE
                   MOVE    1             TO  FLG-JYURRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    "tbl_jyurrk"     TO  MCP-TABLE
           MOVE    "key6"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE  JYURRK-SRYYMD(7:2) TO  IDX7G
      *****07/09/20  4.0対応  Satoh  START*****
      *     MOVE  JYURRK-HOSPID      TO  ACCT-HOSPID  
           MOVE  JYURRK-HOSPNUM      TO  ACCT-HOSPNUM  
      *****07/09/20  4.0対応  Satoh  START*****
           MOVE  JYURRK-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE  JYURRK-PTID        TO  ACCT-PTID
           MOVE  JYURRK-SRYKA       TO  ACCT-SRYKA
           MOVE  JYURRK-SRYYMD(1:6) TO  ACCT-SRYYM
           MOVE  "11"               TO  ACCT-SRYKBN
           MOVE  SRYACCT-REC        TO  MCPDATA-REC
           MOVE  "tbl_sryacct"      TO  MCP-TABLE
           MOVE  "key3"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE  "tbl_sryacct"      TO  MCP-TABLE
               MOVE  "key3"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC TO  SRYACCT-REC
                   MOVE    ZERO        TO  FLG-SRYACCT
               ELSE
                   MOVE    1           TO  FLG-SRYACCT
                   INITIALIZE              SRYACCT-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE  "tbl_sryacct"      TO  MCP-TABLE
           MOVE  "key3"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
      *****07/09/20  4.0対応  Satoh  START*****
      *     CALL    "MONFUNC"           USING
           CALL    "ORCDBMAIN"          USING
      *****07/09/20  4.0対応  Satoh  END*******
                                       MCPAREA
                                       MCPDATA-REC
      *****07/09/20  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/20  4.0対応  Satoh  END*******
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
      *****07/09/20  4.0対応  Satoh  START*****
      *     CALL    "MONFUNC"           USING
           CALL    "ORCDBMAIN"          USING
      *****07/09/20  4.0対応  Satoh  END*******
                                       MCPAREA
                                       MCPDATA-REC
      *****07/09/20  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/20  4.0対応  Satoh  END*******
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    クロース処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
      *****07/09/20  4.0対応  Satoh  START*****
      *     CALL    "MONFUNC"           USING
           CALL    "ORCDBMAIN"          USING
      *****07/09/20  4.0対応  Satoh  END*******
                                       MCPAREA
                                       MCPDATA-REC
      *****07/09/20  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/20  4.0対応  Satoh  END*******
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
