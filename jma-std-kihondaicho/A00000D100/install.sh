#!/bin/sh
#########################################################################
#
# Ģɼ���󥹥ȡ���ġ���(@@@@)
#
# update
# 	2003.09.27
# 		�������� (K.Togazono)
# 	2003.10.07
#		CBL�ե�����򥳥ԡ����ʤ��Х�������
# 		/usr/lib/jma-receipt/site-lib�����
# 		�ե����륳�ԡ��򥳥��ȥ����ȡ�(T.Onuki)
# 	2004.02.20
#       �����Ȥ��ɲá�����
#       ʣ����CBL,INC,dia,red ���б� (Y.Takahashi)
# 	2004.02.25
#       INI �ե�����򥳥ԡ�����褦���ɲ� (Y.Takahashi)
#
#########################################################################


#----------------
# ��� 
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
#(��.red��ޤ�)
FILERED=
#(��.red�ξ��϶�)
FILEDEF=
FILEINI=

#----------------
# ����
#----------------

# Etch�ǻ��Ѥ��륳����UTF-8�ؤ��Ѵ�
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


# root�桼���Ǥʤ��Ȥ���
if test `whoami` != "root"
then
	echomsg "root�桼���Ǽ¹Ԥ��Ƥ�������"
	exit
fi

# Ģɼ�ץ����Ϥɤ���
if test "$DIRTYOHYO" = ""
then
#	echomsg "�裱������Ģɼ�ץ��������Хѥ�����ꤷ�Ƥ�������"
#	exit
    # �����ȥǥ��쥯�ȥ� 
    DIRTYOHYO=./
fi

if ! test -d $DIRTYOHYO
then
	echomsg "Ģɼ�ץ���ब���դ���ޤ��󡣥ѥ����ְ�äƤޤ��󤫡�"
	exit
fi
# (�ѥ��κǸ��"/"�򤯤äĤ��Ƥ���������)
DIRTYOHYO=`echo "$DIRTYOHYO" | sed -e "s/\/$//"`

# "*.CBL"�Ϥ��뤫�� 
FILECBL=`find $DIRTYOHYO -name "*.CBL" -print 2>/dev/null`
if test "$FILECBL" = ""
then
	echo "\"*.CBL\"�ե����뤬���դ���ޤ���"
	exit
fi
# ʣ���ե������б�   (2004/02/20 Y.Takahashi)
FILECBL=`echo $FILECBL | sed -e "s/[\S]*\.\///g"`

# �ץ����̾�򽦤äƤ���
# ʣ���ե������б�   (2004/02/20 Y.Takahashi)
PRGNAME=`echo $FILECBL | sed -e "s/\.CBL//g"`

# ��ʣ�ץ����̾��������
for PRGNAMETMP in $PRGNAME
do
	PRGNAME=`echo $PRGNAME | sed -e "s/$PRGNAMETMP[^ ][^ ]*//g"`
done
# ʣ���ե������б�   (2004/02/20 Y.Takahashi)
for PRGNAMETMP in $PRGNAME
do
#	echo "Program: $PRGNAMETMP"
	FILECBL=`find $DIRTYOHYO -name "$PRGNAMETMP*.CBL" -print 2>/dev/null`
	FILECBL=`echo $FILECBL | sed -e "s/[\S]*\.\///g"`

	# "*.INC"�Ϥ��뤫�� 
	FILEINC=`find $DIRTYOHYO -name "$PRGNAMETMP*.INC" -print 2>/dev/null`
	#INC�ե������̵�����⤢��   (2004/02/20 Y.Takahashi)
	#if test "$FILEINC" = ""
	#then
	#	echo "\"*.INC\"�ե����뤬���դ���ޤ���"
	#	exit
	#fi
	#ʣ���ե������б�   (2004/02/20 Y.Takahashi)
	FILEINC=`echo $FILEINC | sed -e "s/[\S]*\.\///g"`

	# "*.dia"�ޤ���"*.red"�Ϥ��뤫�� 
	FILERED=`find $DIRTYOHYO -name "$PRGNAMETMP*.red" -print 2>/dev/null`
	if test "$FILERED" = ""
	then
		FILERED=`find $DIRTYOHYO -name "$PRGNAMETMP*.dia" -print 2>/dev/null`
	# red,dia ����äƤ��ʤ����⤢��   (2004/02/20 Y.Takahashi)
	#	if test "$FILERED" = ""
	#	then
	#		echo "\"*.dia\"�ޤ���\"*.red\"�ե����뤬���դ���ޤ���"
	#		exit
	#	fi
		# "*.dia"�ϡ�"*.def"�ե��������äƤ���
		FILEDEF=`find $DIRTYOHYO -name "$PRGNAMETMP*.def" -print 2>/dev/null`
		FILEDEF=`echo $FILEDEF | sed -e "s/[\S]*\.\///g"`
	fi
	FILERED=`echo $FILERED | sed -e "s/[\S]*\.\///g"`

	# "*.INI"�����뤫��  (2004/02/25 Y.Takahashi) 
	FILEINI=`find $DIRTYOHYO -name "$PRGNAMETMP*.INI" -print 2>/dev/null`
	#ʣ���ե������б�   
	FILEINI=`echo $FILEINI | sed -e "s/[\S]*\.\///g"`


	# �ե�����ι���������ǧ����
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

	# �Ť��ե������������
	rm -f ${INSCOPY}/${PRGNAME}.so
	# (��ʸ���äƤ��ȤϤʤ����ɡ�����Ȥϸ¤�ʤ�������)
	rm -f ${INSCOPY}/${PRGNAME}.SO
	# (�Ť��С������ϡ�*.red����ʤ��ơ�*.dia�����äƤ����ǽ�������롩)
	# (�� *.red/*.dia�γ�ĥ�Ҥ���ʧ�äƤ����)
	OLDREDNAME=`echo $FILERED | sed -e "s/\..*" 2>/dev/null`
	rm -f ${INSCOPY}/${DIRFORM}/${OLDREDNAME}.dia
	rm -f ${INSCOPY}/${DIRREC}/${OLDREDNAME}.def

done

# ����ѥ����¹Ԥ���
sh /usr/lib/jma-receipt/scripts/allways/site-upgrade.sh
if test $? -ne 0
then
	echomsg "����ѥ���˼��Ԥ��ޤ������������Ƽ¹Ԥ��뤫����ư�����ꤷ�Ƥ���������"
	exit
fi


# ���ԡ����ʤ��Ƥ��ɤ���Τ⤢��Τǡ�
#echo "�Ƕ�Υץ����ϡ�${INSCOPY}�� *.so���򥳥ԡ����ʤ��Ƥ��ɤ���Τ�����ޤ���"
#echo "�ܤ����ϡ���Ģɼ�ץ����� README�򻲾Ȥ��Ʋ�������"
#echo -n "����ѥ��뤷���ե������ ${INSCOPY}�˥��ԡ����ޤ�����[Y/n]: "
#read ANS
#if test "$ANS" = "n" -o "$ANS" = "N"
#then
#	echo "*.so �ե��������򥳥ԡ������˽�λ���ޤ���"	
#	exit
#fi


# �ե�����򥳥ԡ�����
#cp ${INSBASE}/${PRGNAME}.so ${INSCOPY}
#cp ${INSBASE}/${DIRFORM}/${FILERED} ${INSCOPY}/${DIRFORM}

#if test "$FILEDEF" != ""
#then
#	cp ${INSBASE}/${DIRREC}/${FILEDEF} ${INSCOPY}/${DIRREC}
#fi


echomsg "Ģɼ�ץ����Υ��󥹥ȡ����λ���ޤ�"
exit
