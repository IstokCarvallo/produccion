$PBExportHeader$factprod.sra
$PBExportComments$Generated Application Object
forward
global type factprod from application
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
str_paramlote       	gstr_parlote	
str_parempresa		gstr_parempresa	
str_paramplanta		gstr_paramplanta
str_temporada			gstr_paramtempo
str_agronomousuario	gstr_agro
str_temporada			gstr_tempo

String			nom_empresa, rut_empresa, dir_empresa, tel_empresa, &
				gs_CodEmbalaje, gs_base, gs_password, gs_pfijopallet, &
				gs_logoempresa, gs_logoimpresion, gs_logogobierno, is_base, gs_dirres, gs_Ubicacion_DTE, gs_Ubicacion_PDFDTE

Integer		gi_CodExport, gi_CodPlanta, gi_CodEspecie, gi_CodVariedad, gi_admenvase, &
				gi_CodOperacion, gi_CodEmbarque, gi_ctlenvase, gi_emprconex, gi_cliebase, gi_Packing, &
				gi_Emisor_Electronico, gi_Conecion_GuiaElectronica
			
Date			gd_TempoInicio, gd_TempoTermin, gd_fecultsemana

w_informes	vinf

DataStore	ids_archivo,ids_archivo2


string 		gs_menuprincipal, gs_Ambiente = "Windows"
inet			ginet_Base

Long					Sistema_Operativo
uo_ApiWindows	iuo_API
end variables

global type factprod from application
string appname = "factprod"
string themepath = "C:\Program Files (x86)\Appeon\PowerBuilder 22.0\IDE\theme"
string themename = "Flat Design Lime"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\facturacion_productores.ico"
string appruntimeversion = "22.0.0.1900"
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
global factprod factprod

type prototypes

end prototypes

type variables
Constant	Date			id_FechaLiberacion	=	Today()
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on factprod.create
appname="factprod"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on factprod.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;SetPointer ( HourGlass! )

ToolBarPopMenuText		=	"Izquierda,Arriba,Derecha,Abajo,Flotando,Muestra Texto"
MicroHelpDefault			=	"Listo"
gstr_apl.titulo				=	"FACTURACION DE PRODUCTORES"
gstr_apl.ini					=	"FactProd.ini"
gstr_apl.bmp				=	"\Desarrollo 17\Imagenes\Sistemas\facturacion_productores.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\facturacion_productores.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.version			=	"5.22.28022023"
gstr_apl.fechalibera		=  id_FechaLiberacion
//String(id_Version)
gstr_apl.CodigoSistema	=	9
gstr_apl.NombreSistema	=	"Facturación de Productor"

String ls_parametros
ls_parametros				=	CommandParm()

OpenWithParm(w_acceso, ls_parametros)

IF Message.DoubleParm <> 1 THEN
	HALT
	RETURN
END IF

IF AccesoSistemaValido() THEN
	Parempresa()
	ParamPlanta()
	Open(w_main)
ELSE
	HALT
	RETURN
END IF
end event

