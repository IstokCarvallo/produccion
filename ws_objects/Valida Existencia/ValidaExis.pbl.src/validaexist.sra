$PBExportHeader$validaexist.sra
$PBExportComments$Generated Application Object
forward
global type validaexist from application
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
str_parempresa		gstr_parempresa
str_paramplanta		gstr_paramplanta
//str_agronomousuario	gstr_agro
//str_temporada			gstr_paramtempo
//
String			nom_empresa, rut_empresa, &
				dir_empresa, tel_empresa, &
				gs_CodEmbalaje, gs_disco, gs_base, gs_password, is_Base, &
				gs_opcion, gs_windows, gs_emcomext, gs_emsaam,&
				gs_Ambiente = "Windows",gs_pfijopallet, gs_logoempresa, gs_logoimpresion, gs_logogobierno

Integer		gi_CodExport, gi_CodPlanta, gi_CodEspecie, gi_CodVariedad,&
				gi_CodOperacion, gi_CodEmbarque, gi_controlacceso, gi_emprconex, &
				gi_vari_rotulada, gi_Repale, gi_codgen, gi_codtra, gi_packing, gi_stusda, gi_ctlenvase, gi_cliebase, &
				gi_emisor_electronico

Date			gd_TempoInicio, gd_TempoTermin, gd_fecultsemana

inet			ginet_Base

w_informes	vinf

Long				Sistema_Operativo
uo_ApiWindows	iuo_API

end variables

global type validaexist from application
string appname = "validaexist"
string themepath = "C:\Program Files (x86)\Appeon\PowerBuilder 19.0\IDE\theme"
string themename = "Flat Design Blue"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "C:\Desarrollo 17\Imagenes\Sistemas\valida.ico"
string appruntimeversion = "22.0.0.1900"
boolean manualsession = false
boolean unsupportedapierror = false
boolean bignoreservercertificate = false
uint ignoreservercertificate = 0
end type
global validaexist validaexist

type prototypes
FUNCTION ulong		GetCurrentDirectoryA(ulong BufferLen, ref string currentdir) LIBRARY "Kernel32.dll" alias for "GetCurrentDirectoryA;Ansi"
FUNCTION boolean	CopyFileA(ref string cfrom, ref string cto, boolean flag) LIBRARY "Kernel32.dll" alias for "CopyFileA;Ansi"
FUNCTION integer		SQLAllocEnv(ref long henv) LIBRARY "odbc32.dll" 
FUNCTION integer		SQLFreeEnv(long henv) LIBRARY "odbc32.dll" 
FUNCTION integer		SQLDataSources (long henv, integer idirection, ref string szdsn, int idsnmax, ref integer idsn, ref string szdesc, integer idescmax, ref integer idesc) library "odbc32.dll" alias for "SQLDataSources;Ansi" 
FUNCTION boolean	GetUserNameA(ref string uname, ref ulong slength) LIBRARY "ADVAPI32.DLL" alias for "GetUserNameA;Ansi" // Recupera usuario de windows
FUNCTION Boolean	LogonUserA(ref string lpszUsername, ref String lpszDomain, ref string lpszPassword, Long dwLogonType, Long dwLogonProvider, REF  Long  phToken)  LIBRARY "ADVAPI32.DLL" Alias For "LogonUserA;Ansi"
FUNCTION boolean	CloseHandle(long handle)  LIBRARY "kernel32.dll"
FUNCTION ulong		GetLastError() LIBRARY "kernel32.dll"
FUNCTION	Long		SetErrorMode(Ref Long uMode) LIBRARY "kernel32.dll"
FUNCTION	Long		SetFileAttributes(String lpFileSpec , Long dwFileAttributes ) Library "Kernel32.dll" alias for "SetFileAttributesA;Ansi"

end prototypes

type variables
Constant	Date		id_FechaLiberacion	= Date('20190531')
Constant	Time		it_HoraLiberacion		= Now()


end variables

on validaexist.create
appname="validaexist"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on validaexist.destroy
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
gstr_apl.Titulo				=	"Sistema de Validacion Existencia"
gstr_apl.Ini					=	"ValidaExiste.ini"
gstr_apl.Bmp				=	"\Desarrollo 17\Imagenes\Sistemas\valida.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\valida.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.version			=	"5.22.28022023"
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	26
gstr_apl.NombreSistema	=	"Valida Existencia"

String ls_parametros
ls_parametros				=	CommandParm()
//ls_parametros    		=	';Odbc=prod2011;User=pmeza;Pass=asd;Serv=RioOfiCentral11;base=produccion_2011;nrip=192.168.200.251;prto=2738;ubca=d:\bases\produccion_2011.db;driv=C:\Program Files (x86)\Common Files\RunTime\dbodbc11.dll;sist=2;inst=Oficina Central;empr=1'
OpenWithParm(w_acceso, ls_parametros)

IF Message.DoubleParm <> 1 THEN
	HALT
	RETURN
END IF

IF AccesoSistemaValido() THEN
	ParamPlanta()
	Open(w_main)
ELSE
	HALT
	RETURN
END IF
end event

