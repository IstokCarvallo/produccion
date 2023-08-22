$PBExportHeader$estimaciones.sra
forward
global type estimaciones from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
str_aplicacion			gstr_apl
str_usuario 		   		gstr_us
str_temporada			gstr_tempo
str_temporada     		gstr_paramtempo
str_agronomousuario	gstr_agro
str_paramplanta		gstr_paramplanta
str_parametros	  	  	gstr_parametros
str_parempresa		gstr_parempresa

Boolean					ib_tipo_ingreso = False, gb_actualiza_catastro
Long   					Sistema_Operativo, gl_packing

String						nom_empresa, rut_empresa, dir_empresa, tel_empresa, gs_base, is_base, &
							gs_password, gs_dirres,gs_graba, gs_pfijopallet, gs_Ambiente = "Windows", gs_logoempresa,&
							gs_logoimpresion, gs_logogobierno,&
							gs_Ubicacion_DTE, gs_Ubicacion_PDFDTE
							
Integer					gi_Contabilidad, gi_CodExport, gi_tiposel, gi_CodEspecie, gi_emprconex, gi_ctlenvase, gi_codplanta, &
							gi_codvariedad, gi_admenvase, gi_cliebase, gi_Packing, &
							gi_Emisor_Electronico, gi_Conecion_GuiaElectronica
							
datastore				ids_archivo, ids_archivo2

inet						ginet_Base

uo_ApiWindows		iuo_API

w_informes         	     vinf

//Permisos de Archivo
Constant Long A_RDONLY	= 1
Constant Long A_HIDDEN		= 2
Constant Long A_SYSTEM	= 4
Constant Long A_SUBDIR		= 16
Constant Long A_ARCH		= 32
Constant Long A_NORMAL	= 128
end variables

global type estimaciones from application
string appname = "estimaciones"
string themepath = "C:\Program Files (x86)\Appeon\Shared\PowerBuilder\theme190"
string themename = "Do Not Use Themes"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\pronosticos.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global estimaciones estimaciones

type prototypes
//FUNCTION ulong		GetCurrentDirectoryA(ulong BufferLen, ref string currentdir) LIBRARY "Kernel32.dll" alias for "GetCurrentDirectoryA;Ansi"
//FUNCTION boolean	CopyFileA(ref string cfrom, ref string cto, boolean flag) LIBRARY "Kernel32.dll" alias for "CopyFileA;Ansi"
//FUNCTION integer		SQLAllocEnv(ref long henv) LIBRARY "odbc32.dll" 
//FUNCTION integer		SQLFreeEnv(long henv) LIBRARY "odbc32.dll" 
//FUNCTION integer		SQLDataSources (long henv, integer idirection, ref string szdsn, int idsnmax, ref integer idsn, ref string szdesc, integer idescmax, ref integer idesc) library "odbc32.dll" alias for "SQLDataSources;Ansi" 
//FUNCTION boolean	GetUserNameA(ref string uname, ref ulong slength) LIBRARY "ADVAPI32.DLL" alias for "GetUserNameA;Ansi" // Recupera usuario de windows
//FUNCTION Boolean	LogonUserA(ref string lpszUsername, ref String lpszDomain, ref string lpszPassword, Long dwLogonType, Long dwLogonProvider, REF  Long  phToken)  LIBRARY "ADVAPI32.DLL" Alias For "LogonUserA;Ansi"
//FUNCTION boolean	CloseHandle(long handle)  LIBRARY "kernel32.dll"
//FUNCTION ulong		GetLastError() LIBRARY "kernel32.dll"
//FUNCTION Long		CreateProcessWithLogonW(Ref String lpUsername, Ref String lpDomain, Ref String lpPassword, &
//							Ref Long dwLogonFlags, REf String lpApplicationName, Ref String lpCommandLine,&
//							Ref Long dwCreationFlags, Ref Long lpEnvironment, Ref String lpCurrentDirectory, &
//							Ref str_startupinfo lpStartupInfo, Ref str_process_information lpProcessInformation) Library "advapi32.dll" alias for "CreateProcessWithLogonW;Ansi" 
//FUNCTION	Long		SetErrorMode(Ref Long uMode) LIBRARY "kernel32.dll"
////FUNCTION	uLong		GetVersionExA(ref str_osversioninfo lpVersionInfo) LIBRARY "Kernel32.dll" alias for "GetVersionExA;Ansi"
//FUNCTION	Long		SetFileAttributes(String lpFileSpec , Long dwFileAttributes ) Library "Kernel32.dll" alias for "SetFileAttributesA;Ansi"
end prototypes

type variables
Constant	Date		id_FechaLiberacion	= Date('2019-07-22') 
Constant	Time		it_HoraLiberacion		= Now()
end variables

on estimaciones.create
appname="estimaciones"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on estimaciones.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;SetPointer ( HourGlass! )
str_busqueda	lstr_busq

ToolBarPopMenuText		=	"Izquierda,Arriba,Derecha,Abajo,Flotando,Muestra Texto"
MicroHelpDefault			=	"Listo"
gstr_apl.Titulo				=	"SISTEMA ESTIMACIONES DE COSECHA"
gstr_apl.Ini					=	"Pronostico.ini"
gstr_apl.Bmp				=	"\Desarrollo 17\Imagenes\Sistemas\Pronosticos.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\Pronosticos.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.version			=	"5.22.28022023"
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	20
gstr_apl.NombreSistema	=	"Estimación de Cosecha"

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
	ParamTemporada_Tipo(gstr_tempo,4)
	gstr_apl.referencia	=	gstr_tempo.Nombre
	BuscaAgronomo()
	Open(w_main)
	
	IF gstr_Agro.CodigoAgronomo <> 0 And Not IsNull(gstr_Agro.CodigoAgronomo) THEN
		SetPointer(HourGlass!)
		gstr_us.OpcionActiva	=	This.ClassName()
		OpenSheetWithParm(w_consulta_sistemaestimacion, lstr_busq, w_main, 7, Original!)
	END IF
ELSE
	HALT
	RETURN
END IF
end event

