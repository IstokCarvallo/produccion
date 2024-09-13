$PBExportHeader$cajasprod.sra
$PBExportComments$Generated Application Object
forward
global type cajasprod from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables
Str_usuario			gstr_us
Str_paramplanta 	gstr_paramplanta
Str_aplicacion		gstr_apl
str_parempresa 	gstr_parempresa

String				gs_Impresora, gs_TipoAplicac//, gs_empresa = "17800118000083"
Boolean				gb_programado, gb_onfly, gb_conectadosqlca
Long					gl_packing
Integer				gi_manejo
Transaction			itran_odbc
end variables

global type cajasprod from application
string appname = "cajasprod"
string themepath = "C:\Program Files (x86)\Appeon\PowerBuilder 21.0\IDE\theme"
string themename = "Flat Design Dark"
boolean nativepdfvalid = true
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 2
long richtexteditx64type = 3
long richtexteditversion = 1
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\cajas_prod.ico"
string appruntimeversion = "22.2.0.3391"
boolean manualsession = false
boolean unsupportedapierror = false
end type
global cajasprod cajasprod

type prototypes

end prototypes

type variables
String  	is_base
Integer	ii_pos, ii_attempts, ii_seq
Boolean	ib_connected
	

end variables

on cajasprod.create
appname="cajasprod"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on cajasprod.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;String		ls_server1 = "12345678", ls_nombre, ls_Dbms, ls_usuario, ls_clave,ls_DBParm,ls_Base
String		ls_linea, ls_uso, ls_manejo, ls_Provider
Integer	li_posic, li_resp, li_archivo, li_inicio, li_termino

li_archivo		=	FileOpen("Cajasprod.ini")
itran_odbc		=	Create Transaction
Idle(10)

If li_archivo < 0 Then
	MessageBox("Error","Archivo " + "Cajasprod.ini" + " no se encuentra en directorio.", StopSign!)
	Halt Close
Else
		
	DO WHILE FileRead(li_archivo,ls_linea) >= 0 
		If Pos(ls_linea,"Instalación",1) > 0 Then
		End If
		li_inicio	= Pos(ls_linea,"[",1)
		li_termino	= Pos(ls_linea,"]",1)
		
		If li_inicio > 0 AND li_termino>0 Then
			is_base = (Mid(ls_linea, li_inicio + 1, li_termino - li_inicio - 1))
		End If
	LOOP
	
	FileClose(li_archivo)

	itran_odbc.SQLCode	=	1
	ii_pos			=	1
	ls_server1		=	ProfileString("Cajasprod.ini", is_base, "servername", "12345678")
	ls_Dbms			=	ProFileString("Cajasprod.ini", is_base, "dbms", "ODBC")
	ls_DBParm		=	ProfileString("Cajasprod.ini", is_base, "DbParm", "")
	ls_Base			=	ProfileString("Cajasprod.ini", is_base, "database", "")
	ls_usuario        =  ProfileString("Cajasprod.ini", is_base,"LogId", "")
	ls_clave         	=  ProfileString("Cajasprod.ini", is_base,"LogPassWord", "")
	ls_Provider		=	ProfileString("Cajasprod.ini", is_base, "Provider", "SQLNCLI10")

	If ls_server1 <> "12345678" Then
		ls_nombre			=	ProfileString("Cajasprod.ini", is_base, "NombreOdbc", "")
		itran_odbc.Dbms			=	ProFileString("Cajasprod.ini", is_base, "dbms", "ODBC")
		itran_odbc.ServerName	=	ProfileString("Cajasprod.ini", is_base, "servername", "")
		itran_odbc.DataBase		=	ProFileString("Cajasprod.ini", is_base, "database", "")
		gs_Impresora     			=  ProfileString("Cajasprod.ini", is_base,"Impresora", "Sin Referencia")
		gs_TipoAplicac   			=  ProfileString("Cajasprod.ini", is_base,"TipoAplicac", "Sin Referencia")
		ls_uso						=	ProfileString("Cajasprod.ini", is_base,"UsoAplicacion", "AllEnabled")
		ls_manejo					=	ProfileString("Cajasprod.ini", is_base,"ManejoLinea", "Normal")
		
		If ls_Dbms = "ODBC" Then
			
			ls_usuario        			=  ProfileString("Cajasprod.ini", is_base,"nombreTemp1", "Sin Referencia")
			ls_clave          				=  ProfileString("Cajasprod.ini", is_base,"claveTemp1", "Sin Referencia")
	
			itran_odbc.DbParm	=	"ConnectString='DSN=" + ls_nombre + &
									";UID=" + ls_usuario + &
									";PWD=" + ls_clave + "'," + &
									"ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'" + &
									"// ;PBUseProcOwner = " + '"Yes"'
		ElseIf Mid(ls_Dbms,1,3) = 'SNC' or Mid(ls_Dbms,1,9) = 'TRACE SNC' Then
			itran_odbc.LogId   		= ls_usuario
			itran_odbc.LogPass  	= ls_clave
			itran_odbc.Autocommit	= False
				
			If Len(Trim(ls_DBParm)) > 0 Then ls_DbParm = ","+ls_DbParm
			ls_Dbparm = "Provider='"+ls_Provider+"',Database='"+ls_Base+"'"+ls_DbParm+",TrimSpaces=1"	
			itran_odbc.DBParm = ls_Dbparm
		Else
			itran_odbc.LogId			=	ls_usuario
			itran_odbc.LogPass		=	ls_clave
			itran_odbc.Autocommit	=	True
		End If
	Else
		MessageBox("Atención", "No se puede ejecutar la aplicación por la falta de archivo " &
						+ ls_nombre + ".VerIfique que el archivo exista y que esté en el directorio " + &
						"~n~n donde la aplicación esté corriEndo o en el PATH del Computador del cliente.",StopSign!)
		Halt Close
		Return
	End If
End If

If Len(Trim(ls_usuario)) <> 0 Then
	
End If

SetPointer(HourGlass!)
DISCONNECT Using itran_odbc ; 

DO
	CONNECT Using itran_odbc ; 

	If itran_odbc.SQLCode <> 0 Then
		If itran_odbc.SQLDBCode = -103 Then
			If MessageBox("Error de Conexión", "Usuario o Password ingresado está incorrecto.", &
								Information!, RetryCancel!) = 1 Then
			End If
			
			Return
		ElseIf itran_odbc.SQLDBCode = -102 Then
			MessageBox("Error de Conexión", "Demasiados Usuarios conectados a la Base.~r" + &
							"Consulte con Administrador", StopSign!, Ok!)
				Return
		ElseIf itran_odbc.SQLDBCode <> 0 Then
			F_ErrorBaseDatos(itran_odbc, "CajasProd")
			Return
		End If
	End If	
LOOP UNTIL itran_odbc.SQLCode <> 0

ib_connected	= True

If Conexion() Then
	ParamPlanta()
	Open(w_mant_mues_cajasprod_procalm)
End If

SetPointer(Arrow!)
end event

event idle;If gb_ConectadoSQLCA Then
	If IsValid(w_mant_mues_cajasprod_procalm) Then
		//	w_mant_mues_cajasprod_procalm.Calibres()
	End If
Else
	IF Conexion() Then
		IF IsValid(w_mant_mues_cajasprod_procalm) Then
		//	w_mant_mues_cajasprod_procalm.Calibres()
		End If
	End If
End If
end event

