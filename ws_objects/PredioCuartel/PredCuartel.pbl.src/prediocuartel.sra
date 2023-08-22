$PBExportHeader$prediocuartel.sra
$PBExportComments$Generated Application Object
forward
global type prediocuartel from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
str_aplicacion	gstr_apl
str_usuario			gstr_us
str_paramlote		gstr_parlote	
str_paramplanta	gstr_paramplanta
str_parempresa	gstr_parempresa
str_parametros		gstr_parametros
str_agronomousuario	gstr_agro
str_temporada			gstr_paramtempo
str_temporada			gstr_tempo

String 		nom_empresa, rut_empresa, &
		 		dir_empresa, tel_empresa, gs_base, gs_password, gs_DirRes, gs_itgene, gs_prdgen, gs_pfijopallet, &
				 gs_logoempresa, gs_logoimpresion, gs_logogobierno, is_base	, gs_Ubicacion_DTE, gs_Ubicacion_PDFDTE

Integer		gi_CodExport, gi_CodPlanta, gi_CodEspecie, gi_CodVariedad, gi_CodOperacion, gi_CodEmbarque, gi_ctlenvase, gi_Packing, &
				gi_admenvase, gi_emprconex, gi_cliebase,gi_Emisor_Electronico, gi_Conecion_GuiaElectronica
			
Boolean		gb_Repalletizado

Date			gd_TempoInicio, gd_TempoTermin, gd_fecultsemana

Long			gi_CodProductor
string 		gs_menuprincipal, gs_Ambiente = "Windows"
inet			ginet_Base

TRANSACTION sqlcb 
w_informes				vinf

DataStore	ids_archivo,ids_archivo2
Long				Sistema_Operativo
uo_ApiWindows	iuo_API
end variables

global type prediocuartel from application
string appname = "prediocuartel"
string themepath = "C:\Program Files (x86)\Appeon\Shared\PowerBuilder\theme190"
string themename = "Do Not Use Themes"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\predio_cuartel.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global prediocuartel prediocuartel

type prototypes

end prototypes

type variables
Constant	Date			id_FechaLiberacion	=	Date('2020-09-08') 
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on prediocuartel.create
appname="prediocuartel"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on prediocuartel.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;SetPointer(HourGlass!)

ToolBarPopMenuText		=	"Izquierda,Arriba,Derecha,Abajo,Flotando,Muestra Texto"
MicroHelpDefault			=	"Listo"
gstr_apl.titulo				=	"MODULO PREDIOS CUARTELES"
gstr_apl.ini					=	"PredCuartel.ini"
gstr_apl.bmp				=	"\Desarrollo 17\Imagenes\Sistemas\predio_cuartel.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\predio_cuartel.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.version			=	'5.22.28022023'
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	11
gstr_apl.NombreSistema	=	"Predios Cuarteles"

String ls_parametros
ls_parametros				=	CommandParm()
OpenWithParm(w_acceso, ls_parametros)

IF Message.DoubleParm <> 1 THEN
	HALT
	RETURN
END IF

IF AccesoSistemaValido() THEN
	ParEmpresa()
	Parametros()
	ParamTemporada_Tipo(gstr_Tempo, 4)
	Open(w_main)
ELSE
	HALT
	RETURN
END IF
end event

