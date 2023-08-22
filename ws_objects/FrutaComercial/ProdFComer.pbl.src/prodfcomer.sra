$PBExportHeader$prodfcomer.sra
$PBExportComments$Generated Application Object
forward
global type prodfcomer from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
str_aplicacion			gstr_apl
str_usuario				gstr_us
str_parametros			gstr_param
str_paramplanta		gstr_paramplanta
str_temporada     		gstr_paramtempo
str_parempresa		gstr_parempresa
str_agronomousuario	gstr_agro
str_temporada			gstr_tempo

DataStore				ids_archivo,ids_archivo2

String						nom_empresa, rut_empresa, dir_empresa, tel_empresa, gs_pfijopallet, gs_CodEmbalaje, gs_disco, gs_base, &
							gs_password, gs_DirRes, gs_logoempresa, gs_logoimpresion, gs_logogobierno, gs_Ubicacion_DTE, gs_Ubicacion_PDFDTE

Integer     				gi_codexport, gi_CodEspecie, gi_admenvase, gi_ctlenvase, gi_codplanta, &
							gi_todclientes, gi_controlconsol, gi_codvariedad, gi_emprconex, gi_cliebase, gi_Packing, &
				gi_Emisor_Electronico, gi_Conecion_GuiaElectronica
						
String						gs_tipo, is_base,     gs_opcion, &
							gs_windows,  gs_menuprincipal, gs_Ambiente = "Windows"

inet						ginet_Base

w_informes  		vinf
Long					Sistema_Operativo
uo_ApiWindows	iuo_API

end variables

global type prodfcomer from application
string appname = "prodfcomer"
string themepath = "C:\Program Files (x86)\Appeon\Shared\PowerBuilder\theme190"
string themename = "Do Not Use Themes"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\fruta_comercial.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global prodfcomer prodfcomer

type prototypes

end prototypes

type variables
Constant	Date			id_FechaLiberacion	=	Date('2019-07-08')
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on prodfcomer.create
appname="prodfcomer"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on prodfcomer.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;SetPointer ( HourGlass! )

ToolBarPopMenuText		=	"Izquierda,Arriba,Derecha,Abajo,Flotando,Muestra Texto"
MicroHelpDefault			=	"Listo"
gstr_apl.titulo				=	"SISTEMA DE PRODUCCION FRUTA COMERCIAL"
gstr_apl.ini					=	"ProdFComer.ini"
gstr_apl.bmp				=	"\Desarrollo 17\Imagenes\Sistemas\fruta_comercial.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\fruta_comercial.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.version			=	"5.22.28022023"
gstr_apl.CodigoSistema	=	3
gstr_apl.NombreSistema	=	"Producción - Fruta Comercial"

String ls_parametros
ls_parametros				=	CommandParm()
OpenWithParm(w_acceso, ls_parametros)

IF Message.DoubleParm <> 1 THEN
	HALT
	RETURN
END IF

IF AccesoSistemaValido() THEN
	ParEmpresa()
	ParamPlanta()
	gstr_param.plde_codigo	=	gstr_ParamPlanta.CodigoPlanta

	Open(w_main)
ELSE
	HALT
	RETURN
END IF
end event

