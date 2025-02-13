$PBExportHeader$control_calidad.sra
$PBExportComments$Generated Application Object
forward
global type control_calidad from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
str_aplicacion	   	gstr_apl
str_usuario		  	gstr_us
str_ctlcallotes		gstr_Lotes
str_temporada		gstr_tempo

Integer				lar_cuenta,  gi_emprconex
String					gs_tipo, &
						gs_menuprincipal, gs_comprobante, gs_Ambiente = "Windows"

inet					ginet_Base

String					nom_empresa, rut_empresa, dir_empresa, tel_empresa, gs_base, gs_dirres, &
						gs_CodEmbalaje, gs_disco, gs_password, gs_arreglo1, gs_traspasa, &
						gs_opcion, gs_windows,gs_pfijopallet, gs_logoempresa, gs_logoimpresion, gs_logogobierno, is_base, &
						gs_Ubicacion_DTE, gs_Ubicacion_PDFDTE

Integer		  		gi_CodExport, gi_CodPlanta, gi_CodEspecie, gi_CodVariedad, gi_Packing, &
				  	 	gi_CodOperacion, gi_CodEmbarque,  gi_CodZona, gi_CtlEnvase, gi_cliebase, gi_admenvase, &
						gi_Emisor_Electronico, gi_Conecion_GuiaElectronica
Date					gd_TempoInicio, gd_TempoTermin
				
Long           	gi_CodProductor

str_parempresa		gstr_parempresa
str_paramplanta		gstr_paramplanta
str_paramlote			gstr_parlote	
str_temporada			gstr_paramtempo
str_agronomousuario	gstr_agro
w_informes				vinf
DataStore				ids_archivo,ids_archivo2

Long				Sistema_Operativo
uo_ApiWindows	iuo_API
end variables

global type control_calidad from application
string appname = "control_calidad"
string themepath = "C:\Program Files (x86)\Appeon\Shared\PowerBuilder\theme190"
string themename = "Do Not Use Themes"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "F:\Desarrollo 17\Imagenes\Sistemas\control_calidad.ico"
string appruntimeversion = "22.2.0.3397"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global control_calidad control_calidad

type prototypes

end prototypes

type variables
Constant Date			id_FechaLiberacion = Date('2019-07-23')
Constant Time			it_HoraLiberacion  = Now()
end variables

on control_calidad.create
appname="control_calidad"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on control_calidad.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;String ls_Parametros

SetPointer ( HourGlass! )

ToolBarPopMenuText		=	"Izquierda,Arriba,Derecha,Abajo,Flotando,Muestra Texto"
MicroHelpDefault			=	"Listo"
gstr_apl.titulo				=	"MODULO CONTROL DE CALIDAD"
gstr_apl.ini					=	"ConCal.ini"
gstr_apl.bmp				=	"\Desarrollo 17\Imagenes\Sistemas\control_calidad.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\control_calidad.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + String(it_HoraLiberacion)
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.version			=	"5.22.28022023"
gstr_apl.CodigoSistema	=	19
gstr_apl.NombreSistema	=	"Control de Calidad"

ls_parametros				=	CommandParm()
OpenWithParm(w_acceso, ls_parametros)

ParEmpresa()
ParamLote()

IF Message.DoubleParm <> 1 THEN
	HALT
	RETURN
END IF

IF AccesoSistemaValido() THEN
	Open(w_main)
ELSE
	HALT
	RETURN
END IF


end event

