$PBExportHeader$unitec.sra
$PBExportComments$Generated Application Object
forward
global type unitec from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
Str_mant			istr_mant
str_busqueda	istr_busq
Str_aplicacion	gstr_apl
Str_usuario		gstr_us

Transaction gt_UNITEC

Boolean		ib_connected, ib_connected_UNITEC
Integer		ii_pos
String			is_Base

String			gs_Ambiente = "Windows"
end variables

global type unitec from application
string appname = "unitec"
string themepath = "C:\Program Files (x86)\Appeon\PowerBuilder 19.0\IDE\theme"
string themename = "Flat Design Blue"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\UNITEC.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global unitec unitec

type prototypes
FUNCTION ulong FindWindow(ref string  classname, ref string   windowname)  LIBRARY "user32.dll" ALIAS FOR "FindWindowA;ansi"
end prototypes

type variables
Constant	Date			id_FechaLiberacion	=	Date('2019-05-15')
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on unitec.create
appname="unitec"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on unitec.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;ulong hWnd
string ls_title, ls_class

SetPointer (HourGlass!)

gstr_apl.ini	=	"Unitec.ini"
ls_title 		=	"Carga Cajas UNITEC - Granel"
SetNull(ls_class)

hWnd = FindWindow(ls_Class, ls_Title)

If Not IsNull(hWnd) Then Send(hwnd, 16, 0, 0)

gstr_apl.version			=	"5.22.28022023"
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	21
gstr_apl.NombreSistema	=	"SysTray Carga Cajas UNITEC"

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, gstr_us.Computador)

Open(w_cargacajas_systray)
end event

