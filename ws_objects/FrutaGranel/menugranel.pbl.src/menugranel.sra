$PBExportHeader$menugranel.sra
$PBExportComments$Generated Application Object
forward
global type menugranel from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
str_aplicacion		gstr_apl
str_usuario			gstr_us
str_parametros		gstr_param
str_paramplanta	gstr_paramplanta
str_temporada		gstr_paramtempo
str_parempresa		gstr_parempresa

String				nom_empresa, rut_empresa, dir_empresa, tel_empresa, &
						gs_CodEmbalaje, gs_disco, gs_base, gs_password, gs_DirRes, &
						gs_itgene, gs_prdgen, gs_pfijopallet

Integer				gi_CodExport, gi_CodPlanta, gi_CodEspecie, gi_CodVariedad,&
						gi_CodOperacion, gi_CodEmbarque, gi_bodvirtual, gi_bodzonal, &
						gi_admenvase
			
Boolean				gb_Repalletizado, gb_CrtlBins, gb_RecepcionDeProceso
Long 					gi_CodProductor
w_informes			vinf

/*
VARIABLES PARA DLL DE ENVIO DE CORREOS....
EVALUAR CAMBIO
*/
Constant Long SEE_MIN_RESPONSE_WAIT = 1
Constant Long SEE_MAX_RESPONSE_WAIT = 2
Constant Long SEE_CONNECT_WAIT = 3
Constant Long SEE_DISABLE_MIME = 4
Constant Long SEE_QUOTED_PRINTABLE = 8
Constant Long SEE_AUTO_CALL_DRIVER = 9
Constant Long SEE_FILE_PREFIX = 10
Constant Long SEE_SLEEP_TIME = 13
Constant Long SEE_DECODE_UNNAMED = 14
Constant Long SEE_SMTP_PORT = 15
Constant Long SEE_POP3_PORT = 16
Constant Long SEE_MAX_LINE_LENGTH = 17
Constant Long SEE_BLOCKING_MODE = 18
Constant Long SEE_ALLOW_8BITS = 19
Constant Long SEE_LOG_FILE = 20
Constant Long SEE_HIDE_SAVED_MSG = 21
Constant Long SEE_HIDE_TO_ADDR = 22
Constant Long SEE_ADDRESS_DELIMITER = 23
Constant Long SEE_WSACLEANUP = 24
Constant Long SEE_PATH_DELIMITER = 25
Constant Long SEE_ATTACH_DELIMITER = 26
Constant Long SEE_ENABLE_IMAGE = 27
Constant Long SEE_RAW_MODE = 28
Constant Long SEE_ENABLE_ESMTP = 29
Constant Long SEE_ENABLE_APOP = 30
Constant Long SEE_ATTACH_BASE_NUMBER = 31
Constant Long SEE_IGNORE_REJECTED = 32
Constant Long SEE_WRITE_CONTENT_TYPE = 33
Constant Long SEE_SET_FILE_PREFIX = 34
Constant Long SEE_HTML_CHARSET = 35

Constant Long CHARSET_BLANK = 0
Constant Long CHARSET_US_ASCII = 1
Constant Long CHARSET_8859 = 4
Constant Long CHARSET_ISO_8859_1 = 4
Constant Long CHARSET_ISO_8859_8 = 5
Constant Long CHARSET_WIN_1252 = 6
Constant Long CHARSET_WIN_1255 = 7

Constant Long SEE_GET_ERROR_TEXT = 1
Constant Long SEE_GET_COUNTER = 2
Constant Long SEE_GET_RESPONSE = 3
Constant Long SEE_GET_SOCK_ERROR = 4
Constant Long SEE_GET_MESSAGE_BYTES_READ = 10
Constant Long SEE_GET_ATTACH_BYTES_READ = 11
Constant Long SEE_GET_TOTAL_BYTES_READ = 12
Constant Long SEE_GET_MESSAGE_BYTES_SENT = 13
Constant Long SEE_GET_ATTACH_BYTES_SENT = 14
Constant Long SEE_GET_TOTAL_BYTES_SENT = 15
Constant Long SEE_GET_VERSION = 16
Constant Long SEE_GET_MSG_COUNT = 17
Constant Long SEE_GET_MSG_SIZE = 18
Constant Long SEE_GET_BUFFER_COUNT = 19
Constant Long SEE_GET_CONNECT_STATUS = 20
Constant Long SEE_GET_REGISTRATION = 21
Constant Long SEE_GET_ATTACH_COUNT = 22
Constant Long SEE_GET_LAST_RESPONSE = 23
Constant Long SEE_GET_VERIFY_STATUS = 24
Constant Long SEE_GET_SERVER_IP = 25
Constant Long SEE_GET_BUILD = 26
Constant Long SEE_GET_SOCKET = 27
Constant Long SEE_GET_LOCAL_IP = 28
Constant Long SEE_GET_ATTACH_NAMES = 29
Constant Long SEE_GET_LAST_RECIPIENT = 30

Constant Long SEE_COPY_BUFFER = 40
Constant Long SEE_WRITE_BUFFER = 41

Constant Long SEE_SET_REPLY = 50
Constant Long SEE_SET_HEADER = 51
Constant Long SEE_WRITE_TO_LOG = 52
Constant Long SEE_SET_FROM = 53
Constant Long SEE_SET_CONTENT_TYPE = 54
Constant Long SEE_SET_TRANSFER_ENCODING = 55
Constant Long SEE_ADD_HEADER = 56
Constant Long SEE_SET_SECRET = 57
Constant Long SEE_SET_USER = 58
Constant Long SEE_SET_TEXT_MESSAGE = 59
Constant Long SEE_FORCE_INLINE = 60
Constant Long SEE_SET_ATTACH_CONTENT_TYPE = 61
Constant Long SEE_AUTHENTICATE_PROTOCOL = 62
Constant Long SEE_SET_CONTENT_TYPE_PREFIX = 63
Constant Long SEE_ENABLE_XMAILER = 64

Constant Long QUOTED_OFF = 0
Constant Long QUOTED_PLAIN = 1
Constant Long QUOTED_HTML = 2
Constant Long QUOTED_RICH = 3
Constant Long QUOTED_8859 = 4
Constant Long QUOTED_ISO_8859_1 = 4
Constant Long QUOTED_ISO_8859_8 = 5
Constant Long QUOTED_WIN_1252 = 6
Constant Long QUOTED_WIN_1255 = 7
Constant Long QUOTED_USER = 9

Constant Long INLINE_TEXT_OFF = 0
Constant Long INLINE_TEXT_INLINE = 1
Constant Long INLINE_TEXT_ATTACHMENT = 2

Constant Long AUTHENTICATE_CRAM = 1
Constant Long AUTHENTICATE_LOGIN = 2
Constant Long AUTHENTICATE_PLAIN = 4

Constant Long SEE_KEY_CODE = 687283244



end variables
global type menugranel from application
string appname = "menugranel"
end type
global menugranel menugranel

type variables
String  	is_base
Integer	ii_pos, ii_attempts, ii_seq
Boolean	ib_connected
	
Constant	Date			id_FechaLiberacion	=	Date('2011-07-12')//Today()
Constant	Time			it_HoraLiberacion		=	Now()
end variables
on menugranel.create
appname="menugranel"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on menugranel.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;String	ls_server1 = "12345678", ls_nombre, ls_Dbms, ls_usuario, ls_clave
String	ls_linea, ls_uso, ls_manejo
Integer	li_posic, li_resp, li_archivo, li_inicio, li_termino

li_archivo				=	FileOpen("Menugranel.ini")
SQLCa						=	Create Transaction
Idle(10)

SetPointer (HourGlass!)
LONG ll_RC 

ToolBarPopMenuText		=	"Izquierda,Arriba,Derecha,Abajo,Flotando,Muestra Texto"
MicroHelpDefault			=	"Listo"
gstr_apl.titulo			=	"SISTEMA DE PRODUCCION FRUTA GRANEL"
gstr_apl.ini				=	"ProdFGranel.ini"
gstr_apl.bmp				=	"\Desarrollo\Productiva\FrutaGranel\ProdFGranel.bmp"
gstr_apl.Icono				=	"\Desarrollo\Productiva\FrutaGranel\ProdFGranel.ico"
gstr_apl.liberacion		=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.version			=	"3.105.110712"
gstr_apl.fechalibera		=  id_FechaLiberacion

//String(id_Version)
gstr_apl.CodigoSistema	=	2
gstr_apl.NombreSistema	=	"Producción - Fruta Granel"

IF li_archivo < 0 THEN
	MessageBox("Error","Archivo " + "Menugranel.ini" + " no se encuentra en directorio.", StopSign!)
	Halt Close
ELSE
		
	DO WHILE FileRead(li_archivo,ls_linea) >= 0 
		IF Pos(ls_linea,"Instalación",1) > 0 THEN
		END IF
		li_inicio	= Pos(ls_linea,"[",1)
		li_termino	= Pos(ls_linea,"]",1)
		
		IF li_inicio > 0 AND li_termino>0 THEN
			is_base = (Mid(ls_linea, li_inicio + 1, li_termino - li_inicio - 1))
		END IF
	LOOP
	
	FileClose(li_archivo)

	SQLCa.SQLCode	=	1
	ii_pos			=	1
	ls_server1		=	ProfileString("Menugranel.ini", is_base, "servername", "12345678")
	ls_Dbms			=	ProFileString("Menugranel.ini", is_base, "dbms", "ODBC")

	IF ls_server1 <> "12345678" THEN
		ls_nombre			=	ProfileString("Menugranel.ini", is_base, "NombreOdbc", "")
		SQLCa.Dbms			=	ProFileString("Menugranel.ini", is_base, "dbms", "ODBC")
		SQLCa.ServerName	=	ProfileString("Menugranel.ini", is_base, "servername", "")
		SQLCa.DataBase		=	ProFileString("Menugranel.ini", is_base, "database", "")
		ls_usuario        =  ProfileString("Menugranel.ini", is_base,"nombreTemp1", "Sin Referencia")
		ls_clave          =  ProfileString("Menugranel.ini", is_base,"claveTemp1", "Sin Referencia")
		ls_uso				=	ProfileString("Menugranel.ini", is_base,"UsoAplicacion", "AllEnabled")
		ls_manejo			=	ProfileString("Menugranel.ini", is_base,"ManejoLinea", "Normal")
		
		IF ls_Dbms = "ODBC" THEN
			SQLCa.DbParm	=	"ConnectString='DSN=" + ls_nombre + &
									";UID=" + ls_usuario + &
									";PWD=" + ls_clave + "'," + &
									"ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'" + &
									"// ;PBUseProcOwner = " + '"Yes"'
		ELSE
			SQLCa.LogId			=	ls_usuario
			SQLCa.LogPass		=	ls_clave
			SQLCa.Autocommit	=	True
		END IF
	ELSE
		MessageBox("Atención", "No se puede ejecutar la aplicación por la falta de archivo " &
						+ ls_nombre + ".Verifique que el archivo exista y que esté en el directorio " + &
						"~n~n donde la aplicación esté corriendo o en el PATH del Computador del cliente.",StopSign!)
		Halt Close
		Return
	END IF
END IF

IF Len(Trim(ls_usuario)) <> 0 THEN
	
END IF

SetPointer(HourGlass!)

DO
	CONNECT Using SQLCa ; 

	IF SQLCa.SQLCode <> 0 THEN
		IF SQLCa.SQLDBCode = -103 THEN
			IF MessageBox("Error de Conexión", "Usuario o Password ingresado está incorrecto.", &
								Information!, RetryCancel!) = 1 THEN
			END IF
			
			RETURN
		ELSEIF SQLCa.SQLDBCode = -102 THEN
			MessageBox("Error de Conexión", "Demasiados Usuarios conectados a la Base.~r" + &
							"Consulte con Administrador", StopSign!, Ok!)
				RETURN
		ELSEIF SQLCa.SQLDBCode <> 0 THEN
			F_ErrorBaseDatos(SQLCa, "Menugranel")
			RETURN
		END IF
	END IF	
LOOP UNTIL SQLCa.SQLCode <> 0

ib_connected	= True
SetPointer(Arrow!)

Open(w_mant_admausuaropcio_new)
end event

