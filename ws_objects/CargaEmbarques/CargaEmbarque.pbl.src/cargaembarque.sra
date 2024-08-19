$PBExportHeader$cargaembarque.sra
$PBExportComments$Generated Application Object
forward
global type cargaembarque from application
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

global type cargaembarque from application
string appname = "cargaembarque"
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
string appruntimeversion = "22.2.0.3356"
boolean manualsession = false
boolean unsupportedapierror = false
boolean ultrafast = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
long webview2distribution = 0
boolean webview2checkx86 = false
boolean webview2checkx64 = false
string webview2url = "https://developer.microsoft.com/en-us/microsoft-edge/webview2/"
end type
global cargaembarque cargaembarque

type prototypes

end prototypes

type variables
Constant	Date			id_FechaLiberacion	=	Date('2019-01-29')
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on cargaembarque.create
appname="cargaembarque"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on cargaembarque.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;SetPointer (HourGlass!)

gstr_apl.icono				=	"\Desarrollo 17\Imagenes\Sistemas\barco.ico"
gstr_apl.ini					=	"CargaEmbarque.ini"
gstr_apl.version			=	"5.22.10072024"
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	98
gstr_apl.NombreSistema	=	"CargaEmbarque"

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, gstr_us.Computador)

If Conexion() Then Open(w_CargaEmbarque)

SetPointer (Arrow!)
end event

