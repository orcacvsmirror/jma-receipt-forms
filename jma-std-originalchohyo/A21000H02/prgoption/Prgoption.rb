#!/usr/bin/ruby

$:.unshift("/usr/lib/jma-receipt/scripts/daily")

require	"apslib.rb"

def prgoption_select(hospnum,prgid,kbncd)
  prgoption = PandaTable.new(@db,"tbl_prgoption")
  prgoption["tbl_prgoption.HOSPNUM"] = hospnum
  prgoption["tbl_prgoption.PRGID"] = prgid
  prgoption["tbl_prgoption.KBNCD"] = kbncd
  if prgoption.execFunction("DBSELECT","key") == 0
    if prgoption.execFunction("DBFETCH","key") == 0
      rc = 0
    else
      rc = 1
    end
    prgoption.execFunction("DBCLOSECURSOR","key")
  else
    rc = 1
  end
  return rc
end

def prgoption_insert(hospnum,prgid,kbncd,option)
  prgoption = PandaTable.new(@db,"tbl_prgoption")
  t = Time.new
  creymd = t.strftime("%Y%m%d")
  crehms = t.strftime("%H%M%S")
  prgoption["tbl_prgoption.HOSPNUM"] = hospnum
  prgoption["tbl_prgoption.PRGID"] = prgid
  prgoption["tbl_prgoption.KBNCD"] = kbncd
  prgoption["tbl_prgoption.OPTION"] = option
  prgoption["tbl_prgoption.ADDKBN"] = "1"
  prgoption["tbl_prgoption.CREYMD"] = creymd
  prgoption["tbl_prgoption.UPHMS"] = crehms
  prgoption.execFunction("DBINSERT","key")
end

def prgoption_delete(hospnum,prgid,kbncd)
  prgoption = PandaTable.new(@db,"tbl_prgoption")
  prgoption["tbl_prgoption.HOSPNUM"] = hospnum
  prgoption["tbl_prgoption.PRGID"] = prgid
  prgoption["tbl_prgoption.KBNCD"] = kbncd
  prgoption.execFunction("DBDELETE","key")
end

def prgoption_exec(hospnum,prgid,kbncd,filename)
  if kbncd == "EXPLANATION"
    str = "#{prgid} : exp"
  else
    str = "#{prgid} : opt"
  end
  result = prgoption_select(hospnum,prgid,kbncd)
  if result == 0
    $stderr.printf("[ %s ] already exist.\n",str)
    # delete
    ###prgoption_delete(hospnum,prgid,kbncd)
  else
    f = open("#{filename}")
    option = f.read
    f.close
    result = prgoption_insert(hospnum,prgid,kbncd,option)
    if result == 0
      $stderr.printf("[ %s ] succeeded.\n",str)
    else
      $stderr.printf("[ %s ] failed.\n",str)
    end
  end
end

# Main

hospnum = ARGV[0]

@db = PandaDB.new
@db.execFunction("DBOPEN")
@db.execFunction("DBSTART")

$stderr.printf("\nPrgoption start\n\n")

exp_file = Dir::glob("*.exp")
opt_file = Dir::glob("*.opt")
#  $stderr.printf("filename = [ %s ]\n",exp_file)
prgid = File::basename("#{exp_file}",".exp")
$stderr.printf("program id : [ %s ]\n\n",prgid)

kbncd = "EXPLANATION"
prgoption_exec(hospnum,prgid,kbncd,exp_file)

kbncd = "*"
prgoption_exec(hospnum,prgid,kbncd,opt_file)

@db.execFunction("DBCOMMIT")
@db.execFunction("DBDISCONNECT")

$stderr.printf("\nPrgoption end\n")
