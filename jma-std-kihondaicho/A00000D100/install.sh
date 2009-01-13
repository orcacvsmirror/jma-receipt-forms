#!/bin/sh
#########################################################################
#
# 帳票インストールツール(@@@@)
#
# update
# 	2003.09.27
# 		新規作成 (K.Togazono)
# 	2003.10.07
#		CBLファイルをコピーしないバグを修正。
# 		/usr/lib/jma-receipt/site-libからの
# 		ファイルコピーをコメントアウト。(T.Onuki)
# 	2004.02.20
#       コメントの追加、修正
#       複数のCBL,INC,dia,red に対応 (Y.Takahashi)
# 	2004.02.25
#       INI ファイルをコピーするように追加 (Y.Takahashi)
#
#########################################################################


#----------------
# 定数 
#----------------

DIRTYOHYO=$1

DIRCBL=cobol
DIRCPY=${DIRCBL}/copy
DIRFORM=form
DIRREC=record
DIRINI=data

SRCBASE=/usr/local/site-jma-receipt
INSBASE=/usr/lib/jma-receipt/site-lib
INSCOPY=/usr/lib/jma-receipt/

PRGNAME=

FILESO=
FILECBL=
FILEINC=
#(↓.redも含む)
FILERED=
#(↓.redの場合は空)
FILEDEF=
FILEINI=

#----------------
# 処理
#----------------

# Etchで使用するコードUTF-8への変換
# echo message (default character-code EUC)
function echomsg() {
  if [ "${LANG}" = "ja_JP.UTF-8" ] || [ "${LANG}" = "ja_JP.UTF8" ]; then
    if [ -z $2 ]; then
      echo `echo $1 | nkf -wE`
    else
      echo -n `echo $1 | nkf -wE`
    fi 
  else
    if [ -z $2 ]; then
      echo $1
    else
      echo -n $1
    fi
  fi
  return 0
}


# rootユーザでないとだめ
if test `whoami` != "root"
then
	echomsg "rootユーザで実行してください"
	exit
fi

# 帳票プログラムはどこ？
if test "$DIRTYOHYO" = ""
then
#	echomsg "第１引数に帳票プログラムの絶対パスを指定してください"
#	exit
    # カレントディレクトリ 
    DIRTYOHYO=./
fi

if ! test -d $DIRTYOHYO
then
	echomsg "帳票プログラムが見付かりません。パスが間違ってませんか？"
	exit
fi
# (パスの最後に"/"をくっつけてたら削除する)
DIRTYOHYO=`echo "$DIRTYOHYO" | sed -e "s/\/$//"`

# "*.CBL"はあるか？ 
FILECBL=`find $DIRTYOHYO -name "*.CBL" -print 2>/dev/null`
if test "$FILECBL" = ""
then
	echo "\"*.CBL\"ファイルが見付かりません"
	exit
fi
# 複数ファイル対応   (2004/02/20 Y.Takahashi)
FILECBL=`echo $FILECBL | sed -e "s/[\S]*\.\///g"`

# プログラム名を拾ってくる
# 複数ファイル対応   (2004/02/20 Y.Takahashi)
PRGNAME=`echo $FILECBL | sed -e "s/\.CBL//g"`

# 重複プログラム名を削除する
for PRGNAMETMP in $PRGNAME
do
	PRGNAME=`echo $PRGNAME | sed -e "s/$PRGNAMETMP[^ ][^ ]*//g"`
done
# 複数ファイル対応   (2004/02/20 Y.Takahashi)
for PRGNAMETMP in $PRGNAME
do
#	echo "Program: $PRGNAMETMP"
	FILECBL=`find $DIRTYOHYO -name "$PRGNAMETMP*.CBL" -print 2>/dev/null`
	FILECBL=`echo $FILECBL | sed -e "s/[\S]*\.\///g"`

	# "*.INC"はあるか？ 
	FILEINC=`find $DIRTYOHYO -name "$PRGNAMETMP*.INC" -print 2>/dev/null`
	#INCファイルは無い時もある   (2004/02/20 Y.Takahashi)
	#if test "$FILEINC" = ""
	#then
	#	echo "\"*.INC\"ファイルが見付かりません"
	#	exit
	#fi
	#複数ファイル対応   (2004/02/20 Y.Takahashi)
	FILEINC=`echo $FILEINC | sed -e "s/[\S]*\.\///g"`

	# "*.dia"または"*.red"はあるか？ 
	FILERED=`find $DIRTYOHYO -name "$PRGNAMETMP*.red" -print 2>/dev/null`
	if test "$FILERED" = ""
	then
		FILERED=`find $DIRTYOHYO -name "$PRGNAMETMP*.dia" -print 2>/dev/null`
	# red,dia を持っていない時もある   (2004/02/20 Y.Takahashi)
	#	if test "$FILERED" = ""
	#	then
	#		echo "\"*.dia\"または\"*.red\"ファイルが見付かりません"
	#		exit
	#	fi
		# "*.dia"は、"*.def"ファイルも持っている
		FILEDEF=`find $DIRTYOHYO -name "$PRGNAMETMP*.def" -print 2>/dev/null`
		FILEDEF=`echo $FILEDEF | sed -e "s/[\S]*\.\///g"`
	fi
	FILERED=`echo $FILERED | sed -e "s/[\S]*\.\///g"`

	# "*.INI"があるか？  (2004/02/25 Y.Takahashi) 
	FILEINI=`find $DIRTYOHYO -name "$PRGNAMETMP*.INI" -print 2>/dev/null`
	#複数ファイル対応   
	FILEINI=`echo $FILEINI | sed -e "s/[\S]*\.\///g"`


	# ファイルの更新時刻を確認する
	for FILECBLTMP in $FILECBL
	do
		if ! find ${SRCBASE}/${DIRCBL}/${FILECBLTMP} 2>/dev/null -o \
		find ${DIRTYOHYO}/${FILECBLTMP} -newer ${SRCBASE}/${DIRCBL}/${FILECBLTMP} 2>/dev/null
		then
			echo "cp ${DIRTYOHYO}/${FILECBLTMP} ${SRCBASE}/${DIRCBL}"
			if ! cp ${DIRTYOHYO}/${FILECBLTMP} ${SRCBASE}/${DIRCBL}
			then
				exit
			fi
		fi
	done

	for FILEINCTMP in $FILEINC
	do
		if ! find ${SRCBASE}/${DIRCPY}/${FILEINCTMP} 2>/dev/null -o \
			find ${DIRTYOHYO}/${FILEINCTMP} -newer ${SRCBASE}/${DIRCPY}/${FILEINCTMP} 2>/dev/null
		then
			echo "cp ${DIRTYOHYO}/${FILEINCTMP} ${SRCBASE}/${DIRCPY}"
			if ! cp ${DIRTYOHYO}/${FILEINCTMP} ${SRCBASE}/${DIRCPY}
			then
	   	         exit
	       	fi
		fi
	done

	for FILEREDTMP in $FILERED
	do
		if ! find ${SRCBASE}/${DIRFORM}/${FILEREDTMP} 2>/dev/null -o \
			find ${DIRTYOHYO}/${FILEREDTMP} -newer ${SRCBASE}/${DIRFORM}/${FILEREDTMP} 2>/dev/null
		then
			echo "cp ${DIRTYOHYO}/${FILEREDTMP} ${SRCBASE}/${DIRFORM}"
			if ! cp ${DIRTYOHYO}/${FILEREDTMP} ${SRCBASE}/${DIRFORM}
	        then
				exit
	        fi
		fi
	done

	for FILEINITMP in $FILEINI
	do
		if ! find ${SRCBASE}/${DIRINI}/${FILEINITMP} 2>/dev/null -o \
		find ${DIRTYOHYO}/${FILEINITMP} -newer ${SRCBASE}/${DIRINI}/${FILEINITMP} 2>/dev/null
		then
			echo "cp ${DIRTYOHYO}/${FILEINITMP} ${SRCBASE}/${DIRINI}"
			if ! cp ${DIRTYOHYO}/${FILEINITMP} ${SRCBASE}/${DIRINI}
			then
				exit
			fi
		fi
	done
	
	if test "$FILEDEF" != ""
	then
		for FILEDEFTMP in $FILEDEF
		do
			if ! find ${SRCBASE}/${DIRREC}/${FILEDEFTMP} 2>/dev/null -o \
				find ${DIRTYOHYO}/${FILEDEFTMP} -newer ${SRCBASE}/${DIRREC}/${FILEDEFTMP} 2>/dev/null
			then
				echo "cp ${DIRTYOHYO}/${FILEDEFTMP} ${SRCBASE}/${DIRREC}"
				if ! cp ${DIRTYOHYO}/${FILEDEFTMP} ${SRCBASE}/${DIRREC}
		        then
		 	         exit
		        fi
			fi
		done
	fi

	# 古いファイルを削除する
	rm -f ${INSCOPY}/${PRGNAME}.so
	# (大文字ってことはないけど、ゼロとは限らない、、、)
	rm -f ${INSCOPY}/${PRGNAME}.SO
	# (古いバージョンは、*.redじゃなくて、*.diaが入っている可能性がある？)
	# (↓ *.red/*.diaの拡張子を取り払ってるだけ)
	OLDREDNAME=`echo $FILERED | sed -e "s/\..*" 2>/dev/null`
	rm -f ${INSCOPY}/${DIRFORM}/${OLDREDNAME}.dia
	rm -f ${INSCOPY}/${DIRREC}/${OLDREDNAME}.def

done

# コンパイルを実行する
sh /usr/lib/jma-receipt/scripts/allways/site-upgrade.sh
if test $? -ne 0
then
	echomsg "コンパイルに失敗しました。シェルを再実行するか、手動で設定してください。"
	exit
fi


# コピーしなくても良いものもあるので。
#echo "最近のプログラムは、${INSCOPY}に *.so等をコピーしなくても良いものがあります。"
#echo "詳しくは、各帳票プログラムの READMEを参照して下さい。"
#echo -n "コンパイルしたファイルを ${INSCOPY}にコピーしますか？[Y/n]: "
#read ANS
#if test "$ANS" = "n" -o "$ANS" = "N"
#then
#	echo "*.so ファイル等をコピーせずに終了します。"	
#	exit
#fi


# ファイルをコピーする
#cp ${INSBASE}/${PRGNAME}.so ${INSCOPY}
#cp ${INSBASE}/${DIRFORM}/${FILERED} ${INSCOPY}/${DIRFORM}

#if test "$FILEDEF" != ""
#then
#	cp ${INSBASE}/${DIRREC}/${FILEDEF} ${INSCOPY}/${DIRREC}
#fi


echomsg "帳票プログラムのインストールを終了します"
exit
