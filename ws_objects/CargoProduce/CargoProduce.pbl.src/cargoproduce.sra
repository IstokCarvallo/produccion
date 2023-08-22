$PBExportHeader$cargoproduce.sra
$PBExportComments$Generated Application Object
forward
global type cargoproduce from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
Str_aplicacion		gstr_apl
Str_usuario			gstr_us
str_parempresa 	gstr_parempresa

String 	is_Base
INteger	ii_Pos
Boolean	ib_Connected
end variables

global type cargoproduce from application
string appname = "cargoproduce"
string themepath = "C:\Program Files (x86)\Appeon\PowerBuilder 21.0\IDE\theme"
string themename = "Flat Design Silver"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\barco.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global cargoproduce cargoproduce

type prototypes

end prototypes

type variables
Constant	Date			id_FechaLiberacion	=	Date('2019-01-29')
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on cargoproduce.create
appname="cargoproduce"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on cargoproduce.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;SetPointer (HourGlass!)

gstr_apl.icono				=	"\Desarrollo 17\Imagenes\Sistemas\barco.ico"
gstr_apl.ini					=	"CargoProduce.ini"
gstr_apl.version			=	"5.22.28022023"
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	98
gstr_apl.NombreSistema	=	"CargoProduce"

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, gstr_us.Computador)

If Conexion() Then Open(w_cargoproduce)

SetPointer (Arrow!)
end event

