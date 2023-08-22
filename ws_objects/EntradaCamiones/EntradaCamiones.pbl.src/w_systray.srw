$PBExportHeader$w_systray.srw
$PBExportComments$Ancestro  Ventana systray
forward
global type w_systray from window
end type
type notifyicondata from structure within w_systray
end type
end forward

type notifyicondata from structure
	long		cbsize
	long		hwnd
	long		uid
	long		uflags
	long		ucallbackmessage
	long		hicon
	character		sztip[64]
end type

global type w_systray from window
string tag = "Capturador Llamadas Telefónicas"
integer x = 1518
integer y = 788
integer width = 2016
integer height = 1208
boolean titlebar = true
string title = "Capturador"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowstate windowstate = maximized!
long backcolor = 16777215
boolean clientedge = true
event command pbm_command
event syscommand pbm_syscommand
event ue_traydoubleclicked ( )
event ue_trayrightclicked ( )
end type
global w_systray w_systray

type prototypes
FUNCTION integer Shell_NotifyIcon (long dwMessage, REF NOTIFYICONDATA lpData) LIBRARY "shell32.dll" ALIAS FOR "Shell_NotifyIconA;Ansi"
FUNCTION long LoadIcon(long AppHandle, string Resource) LIBRARY "USER32.DLL" Alias For "LoadIconA;Ansi"
end prototypes

type variables


Private:
NOTIFYICONDATA IconData

// Systray messages
CONSTANT integer NIM_ADD 	= 0
CONSTANT integer NIM_MODIFY 	= 1
CONSTANT integer NIM_DELETE 	= 2
CONSTANT integer NIF_MESSAGE 	= 1
CONSTANT integer NIF_ICON 	= 2
CONSTANT integer NIF_TIP 		= 4

// Windows messages
CONSTANT long WM_COMMAND 		= 273
CONSTANT long WM_GETICON 		= 127
CONSTANT long WM_LBUTTONDBLCLK 	= 515
CONSTANT long WM_RBUTTONUP		= 517
CONSTANT long SC_CLOSE 			= 61536
CONSTANT long SC_MINIMIZAR 	= 61472


end variables

forward prototypes
public subroutine conexion ()
end prototypes

event command;CHOOSE CASE Message.LongParm

	CASE WM_LBUTTONDBLCLK
 		This.TriggerEvent("ue_traydoubleclicked")
	CASE WM_RBUTTONUP
 		This.TriggerEvent("ue_trayrightclicked")

END CHOOSE
end event

event syscommand;IF CommandType = SC_CLOSE OR CommandType = SC_MINIMIZAR THEN
	Hide(This)
	Message.Processed 	= True
	Message.ReturnValue 	= 1
END IF
end event

event ue_traydoubleclicked();//istr_mant.Argumento[1]	=	"Control Trafico Telefónico"
//istr_mant.Argumento[2]	=	istr_config.Password
//
//OpenWithParm(w_passwordCentral, istr_mant)
//
//istr_mant	=	Message.PowerObjectParm
//
//IF istr_mant.Respuesta = 0 THEN 
//	MessageBox("Atención","Clave Errónea",Exclamation!)
//	//Close(This)
//ELSEIF istr_mant.Respuesta = 1 THEN
	Show(This)
	This.POST SetFocus()
//END IF
end event

public subroutine conexion ();String		ls_server1 = "12345678", ls_nombre, ls_Dbms, ls_DbParm, ls_Provider

IF ib_connected THEN
	DISCONNECT Using SQLCA ;
END IF

ib_connected	=	False

sqlca.SQLCode	=	1
ii_pos			=	1
ls_server1		=	ProfileString(gstr_apl.ini, is_base, "servername", "12345678")
ls_Dbms			=	ProFileString(gstr_apl.ini, is_base, "dbms", "ODBC")
ls_Provider		=	ProfileString(gstr_apl.ini, is_base, "Provider", "SQLNCLI10")

IF ls_server1 <> "12345678" THEN
	ls_nombre			=	ProfileString(gstr_apl.ini, is_base, "NombreOdbc", "")
	sqlca.Dbms			=	ProFileString(gstr_apl.ini, is_base, "dbms", "ODBC")
	sqlca.ServerName	=	ProfileString(gstr_apl.ini, is_base, "servername", "")
	sqlca.DataBase		=	ProFileString(gstr_apl.ini, is_base, "database", "")
	sqlca.LogId			=	ProFileString(gstr_apl.ini, is_base, "LogId", "dba")
	sqlca.LogPass		=	ProFileString(gstr_apl.ini, is_base, "LogPassWord", "info")
	
	IF ls_Dbms = "ODBC" THEN
		sqlca.DbParm	=	"ConnectString='DSN=" + ls_nombre + &
								";UID=" + sqlca.LogId + &
								";PWD=" + sqlca.LogPass + "'," + &
								"ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'" + &
								"// ;PBUseProcOwner = " + '"Yes"'
	ElseIf Mid(ls_Dbms,1,3) = 'SNC' or Mid(ls_Dbms,1,9) = 'TRACE SNC' Then
		sqlca.Autocommit = True
			
		If Len(Trim(ls_DBParm)) > 0 Then ls_DbParm = ","+ls_DbParm
		
		ls_Dbparm = "Provider='" + ls_Provider + "',Database='"+ProfileString(gstr_apl.ini, is_base, "database", "")+"'"+ls_DbParm+",TrimSpaces=1"
			
		SQLCA.DBParm = ls_Dbparm
		
	ELSE
		sqlca.LogId			=	'dba'
		sqlca.LogPass		=	'info'
		sqlca.Autocommit	=	True
	END IF
ELSE
	MessageBox(This.Title,"No se puede ejecutar la aplicación por la falta de archivo " &
					+ gstr_apl.ini + ". Verifique que el archivo exista y que esté en el directorio " + &
					"donde la aplicación esté corriendo o en el PATH del Computador del cliente.",StopSign!)
	Halt Close
	Return
END IF

SetPointer(HourGlass!)

CONNECT Using SQLCA ; 

IF sqlca.SQLCode = 0 THEN
	ib_connected	= True
	
ELSE
	ib_connected	= False
	
END IF



end subroutine

on w_systray.create
end on

on w_systray.destroy
end on

event open;This.icon	=	"\Desarrollo 17\Imagenes\Sistemas\Maquinarias.ico"

nvo_SizeOf 	Size
long 			ll_Icon
String			ls_linea
Integer		li_posic, li_resp, li_archivo, li_inicio, li_termino

SetPointer(HourGlass!)

//Show(This)
//
ll_Icon 							= 	Send(Handle(This),WM_GETICON,1,0)

IconData.cbSize				= 	Size.SizeOf(IconData)
IconData.hWnd					= 	Handle(This)
IconData.uID					= 	1
IconData.uFlags				= 	BitOr({NIF_ICON,NIF_MESSAGE,NIF_TIP})
IconData.uCallBackMessage	= 	WM_COMMAND
IconData.hIcon					= 	ll_Icon
IconData.szTip					= 	This.Title

IF Shell_NotifyIcon(NIM_ADD,IconData) = 0 THEN
	MessageBox("Error en Systray", "Imposible Agregar Systray icon!",StopSign!,Ok!)
END IF

//Hide(This)
//

li_archivo						=	FileOpen(gstr_apl.Ini)

IF li_archivo < 0 THEN
	MessageBox("Error","Archivo " + gstr_apl.ini + " no se encuentra en directorio.", StopSign!)
	Halt Close
ELSE
	SetPointer(HourGlass!)

	DO WHILE FileRead(li_archivo,ls_linea) >= 0
		IF Pos(ls_linea,"Instalación",1) > 0 THEN
		//	st_empresa.text		=	Mid(ls_linea,13,Len(ls_linea)-12)
			//gstr_apl.instalacion	=	st_empresa.text
		END IF
		
		li_inicio	= Pos(ls_linea,"[",1)
		li_termino	= Pos(ls_linea,"]",1)
		
		IF li_inicio > 0 AND li_termino>0 THEN
			is_Base = (Mid(ls_linea, li_inicio + 1, li_termino - li_inicio - 1))
		END IF
	LOOP

	FileClose(li_archivo)

END IF

conexion()

SetPointer(Arrow!)
end event

event close;IF Shell_NotifyIcon(NIM_DELETE,IconData) = 0 THEN
	MessageBox("Systray error", "Imposible Quitar Systray icon!",StopSign!,Ok!)
END IF
end event

