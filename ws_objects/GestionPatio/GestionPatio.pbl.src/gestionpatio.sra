$PBExportHeader$gestionpatio.sra
$PBExportComments$Generated Application Object
forward
global type gestionpatio from application
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
Str_paramplanta 	gstr_paramplanta
str_parempresa 	gstr_parempresa

String 	is_Base
INteger	ii_Pos
Boolean	ib_Connected
end variables

global type gestionpatio from application
string appname = "gestionpatio"
string themepath = "C:\Program Files (x86)\Appeon\PowerBuilder 21.0\IDE\theme"
string themename = "Flat Design Blue"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 0
long richtexteditx64type = 3
long richtexteditversion = 0
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\Control.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = true
end type
global gestionpatio gestionpatio

type prototypes

end prototypes

type variables
Constant	Date			id_FechaLiberacion	=	Date('2019-01-29')
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on gestionpatio.create
appname="gestionpatio"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on gestionpatio.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;SetPointer (HourGlass!)

gstr_apl.icono				=	"\Desarrollo 17\Imagenes\Sistemas\Control.ico"
gstr_apl.ini					=	"GestionPatio.ini"
gstr_apl.version			=	"5.21.30092021"
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	98
gstr_apl.NombreSistema	=	"Consola Frio"

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
				"ComputerName", RegString!, gstr_us.Computador)

If Conexion() Then
	ParamPlanta()	
	Open(w_gestionpatio)
End If

SetPointer (Arrow!)
end event

