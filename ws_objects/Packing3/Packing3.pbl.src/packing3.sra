$PBExportHeader$packing3.sra
$PBExportComments$Generated Application Object
forward
global type packing3 from application
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
str_temporada			gstr_paramtempo
str_parempresa		gstr_parempresa
str_agronomousuario	gstr_agro

String					nom_empresa, rut_empresa, dir_empresa, tel_empresa, &
						gs_CodEmbalaje, gs_disco, gs_base, gs_password, gs_DirRes, &
						gs_itgene, gs_prdgen, gs_pfijopallet,     gs_menuprincipal, gs_Ambiente = "Windows",&
						gs_logoempresa, gs_logoimpresion, gs_logogobierno, is_base, gs_Ubicacion_DTE, gs_Ubicacion_PDFDTE

Integer				gi_CodExport, gi_CodPlanta, gi_CodEspecie, gi_CodVariedad,&
						gi_CodOperacion, gi_CodEmbarque, gi_bodvirtual, gi_bodzonal, &
						gi_admenvase, gi_ctlenvase, gi_emprconex, gi_cliebase, gi_Packing, gi_Emisor_Electronico, gi_Conecion_GuiaElectronica
			
Boolean				gb_Repalletizado, gb_CrtlBins, gb_RecepcionDeProceso, gb_memo
Long 					gi_CodProductor
w_informes			vinf

inet					ginet_Base

DataStore	ids_archivo,ids_archivo2

Long				Sistema_Operativo
uo_ApiWindows	iuo_API



end variables

global type packing3 from application
string appname = "packing3"
string themepath = "C:\Program Files (x86)\Appeon\PowerBuilder 22.0\IDE\theme"
string themename = "Do Not Use Themes"
boolean nativepdfvalid = false
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\fruta_granel_pterceros.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global packing3 packing3

type prototypes


end prototypes
type variables
Constant	Date			id_FechaLiberacion	=	Date('2019-02-12')
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on packing3.create
appname="packing3"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on packing3.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;String ls_parametros

SetPointer (HourGlass!)

ToolBarPopMenuText		=	"Izquierda,Arriba,Derecha,Abajo,Flotando,Muestra Texto"
MicroHelpDefault			=	"Listo"
gstr_apl.titulo				=	"SISTEMA DE PRODUCCION FRUTA GRANEL PACKING TERCEROS"
gstr_apl.ini					=	"Packing3.ini"
gstr_apl.bmp				=	"\Desarrollo 17\Imagenes\Sistemas\fruta_granel_pterceros.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\fruta_granel_pterceros.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.version			=	"5.22.28022023"
gstr_apl.fechalibera		=  id_FechaLiberacion

//String(id_Version)
gstr_apl.CodigoSistema	=	27
gstr_apl.NombreSistema	=	"Producción - Fruta Granel Packing Terceros"
ls_parametros				=	CommandParm()
OpenWithParm(w_acceso, ls_parametros)


IF Message.DoubleParm <> 1 THEN
	HALT
	RETURN
END IF

IF AccesoSistemaValido() THEN
	Parempresa()
	ParamPlanta()
	gstr_param.plde_codigo	=	gstr_ParamPlanta.CodigoPlanta
	Open(w_main)
ELSE
	HALT
	RETURN
END IF

end event

