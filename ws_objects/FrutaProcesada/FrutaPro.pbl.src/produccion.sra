$PBExportHeader$produccion.sra
forward
global type produccion from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
uo_ApiWindows		iuo_API

str_aplicacion			gstr_apl
str_usuario				gstr_us
str_paramlote        	gstr_parlote	
Str_mant					istr_mant3
str_parempresa		gstr_parempresa
str_paramplanta		gstr_paramplanta
str_temporada			gstr_paramtempo
str_agronomousuario	gstr_agro
str_temporada			gstr_tempo

String			nom_empresa, rut_empresa, dir_empresa, tel_empresa, gs_CodEmbalaje, gs_disco, &
				gs_base, gs_password, gs_opcion, gs_windows, gs_emcomext, gs_emsaam, gs_clavecomext, &
				gs_pfijopallet,gs_DirRes, gs_Ambiente = 'Windows', gs_logoempresa, gs_logoimpresion, gs_logogobierno,&
				is_base, gs_Ubicacion_DTE, gs_Ubicacion_PDFDTE
				
inet			ginet_Base

Integer		gi_CodExport, gi_CodPlanta, gi_CodEspecie, gi_CodVariedad,gi_CodOperacion, gi_CodEmbarque, gi_controlacceso, &
				gi_vari_rotulada, gi_Repale, gi_codgen, gi_codtra, gi_packing, gi_stusda, gi_Rece_Caja, gi_todclientes, gi_cali_rotulado, &
				gi_pack_rotulado, gi_prod_rotulado,gi_ctlenvase, gi_admenvase, gi_emprconex, gi_cliebase, gi_controlconsol, &
				gi_Emisor_Electronico, gi_Conecion_GuiaElectronica
			
Boolean		gb_Repalletizado, gb_Memo = True
Date			gd_TempoInicio, gd_TempoTermin, gd_fecultsemana
Long			gi_CodProductor, Sistema_Operativo

TRANSACTION sqlcb 
w_informes				vinf

DataStore	ids_archivo,ids_archivo2
end variables
global type produccion from application
string appname = "produccion"
string themepath = "C:\Program Files (x86)\Appeon\Shared\PowerBuilder\theme190"
string themename = "Do Not Use Themes"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\fruta_procesada.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global produccion produccion

type prototypes

end prototypes

type variables
Constant	Date			id_FechaLiberacion	=	Date('2019-08-01')
Constant	Time			it_HoraLiberacion		=	Now()
end variables

on produccion.create
appname="produccion"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on produccion.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open; SetPointer ( HourGlass! )

ToolBarPopMenuText		=	"Izquierda,Arriba,Derecha,Abajo,Flotando,Muestra Texto"
MicroHelpDefault			=	"Listo"
gstr_apl.titulo				=	"SISTEMA FRUTA PROCESADA"
gstr_apl.ini					=	"FrutaPro.ini"
gstr_apl.bmp				=	"\Desarrollo 17\Imagenes\Sistemas\fruta_procesada.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\fruta_procesada.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.version			=	"5.22.28022023"
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	1
gstr_apl.NombreSistema	=	"Fruta Procesada"

String ls_parametros
ls_parametros				=	CommandParm()

OpenWithParm(w_acceso, ls_parametros)

IF Message.DoubleParm <> 1 THEN
	HALT
	RETURN
END IF

IF AccesoSistemaValido() THEN
	ParEmpresa()
	Open(w_main)
ELSE
	HALT
	RETURN
END IF
end event

