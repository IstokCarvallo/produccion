$PBExportHeader$certificaciones.sra
forward
global type certificaciones from application
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
str_agronomousuario	gstr_agro
str_parempresa		gstr_parempresa
str_paramplanta		gstr_paramplanta
str_temporada			gstr_tempo
str_temporada			gstr_paramtempo

String			nom_empresa, rut_empresa, &
				dir_empresa, tel_empresa, &
				gs_CodEmbalaje, gs_disco, gs_base, gs_password, &
				gs_opcion, gs_windows, gs_emcomext, gs_emsaam, gs_Ambiente = "Windows",&
				gs_pfijopallet, gs_logoempresa, gs_logoimpresion, gs_logogobierno, is_base, gs_dirres


Integer		gi_CodExport, gi_CodPlanta, gi_CodEspecie, gi_CodVariedad,&
				gi_CodOperacion, gi_CodEmbarque, gi_controlacceso, gi_emprconex, &
				gi_vari_rotulada, gi_Repale, gi_codgen, gi_codtra, gi_packing, gi_stusda, &
				gi_ctlenvase, gi_cliebase, gi_emisor_electronico, gi_admenvase

DataStore	ids_archivo,ids_archivo2				


inet			ginet_Base

Date			gd_TempoInicio, gd_TempoTermin, gd_fecultsemana

w_informes	vinf

Long					Sistema_Operativo
uo_ApiWindows	iuo_API
end variables

global type certificaciones from application
string appname = "certificaciones"
string themepath = "C:\Program Files (x86)\Appeon\PowerBuilder 21.0\IDE\theme"
string themename = "Flat Design Blue"
boolean nativepdfvalid = false
boolean nativepdfincludecustomfont = false
string nativepdfappname = ""
long richtextedittype = 5
long richtexteditx64type = 5
long richtexteditversion = 3
string richtexteditkey = ""
string appicon = "\Desarrollo 17\Imagenes\Sistemas\Certificaciones.ico"
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
global certificaciones certificaciones

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

//Envio de Mail
FUNCTION Integer seeAbort (Integer Chan) LIBRARY "SEE32.DLL"
FUNCTION Long seeAttach (Long NbrChans, Long KeyCode) LIBRARY "SEE32.DLL"
FUNCTION Long seeClose (Long Chan) LIBRARY "SEE32.DLL"
FUNCTION Long seeCommand (Long Chan, String Text) LIBRARY "SEE32.DLL"
FUNCTION Long seeDebug (Long Chan, Long Index, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long seeDecodeBuffer (String CodedBuf, String ClearBuf, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long seeDecodeUTF8 (String UTF8Buffer, String UnicodeBuffer) LIBRARY "SEE32.DLL"
FUNCTION Long seeDecodeUU (String CodedBuf, String ClearBuf) LIBRARY "SEE32.DLL"
FUNCTION Long seeDeleteEmail (Long Chan, Long MsgNbr) LIBRARY "SEE32.DLL"
FUNCTION Long seeDriver (Long Chan) LIBRARY "SEE32.DLL"
FUNCTION Long seeEncodeBuffer (String ClearBuf, String CodedBuf, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long seeEncodeUTF8 (Long UnicodeValue, String UTF8Buffer) LIBRARY "SEE32.DLL"
FUNCTION Long seeErrorText (Long Chan, Long Code, ref String Buffer, Long BufLen) LIBRARY "SEE32.DLL" alias for "seeErrorText;Ansi"
FUNCTION Long seeExtractLine (String Src, Long LineNbr, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long seeExtractText (String Src, String Text, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long seeGetEmailCount (Long Chan) LIBRARY "SEE32.DLL"
FUNCTION Long seeGetEmailFile (Long Chan, Long MsgNbr, String FileName, String EmailDir, String AttachDir) LIBRARY "SEE32.DLL"
FUNCTION Long seeGetEmailLines (Long Chan, Long MsgNbr, Long Lines, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long seeGetEmailSize (Long Chan, Long MsgNbr) LIBRARY "SEE32.DLL"
FUNCTION Long seeGetEmailUID (Long Chan, Long MsgNbr, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long seeIntegerParam (Long Chan, Long Index, Long Value) LIBRARY "SEE32.DLL"
FUNCTION Long seePop3Connect (Long Chan, String Server, String User, String Password) LIBRARY "SEE32.DLL"
FUNCTION Long seeQuoteBuffer (String ClearBuf, String CodedBuf, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long seeRelease () LIBRARY "SEE32.DLL"
FUNCTION Long seeSendEmail (Long Chan, String Rcpt, String CC, String BCC, String Subj, String Msg, String Attach) LIBRARY "SEE32.DLL" alias for "seeSendEmail;Ansi"
FUNCTION Long seeSendHTML (Long Chan, String Rcpt, String CC, String BCC, String Subj, String Msg, String Images, String AltTxt, String Attach) LIBRARY "SEE32.DLL"
FUNCTION Long seeSmtpConnect (Long Chan, String Server, String Fromm, String Reply) LIBRARY "SEE32.DLL" alias for "seeSmtpConnect;Ansi"
FUNCTION Long seeStatistics (Long Chan, Long Index) LIBRARY "SEE32.DLL"
FUNCTION Long seeStringParam (Long Chan, Long Index, String Value) LIBRARY "SEE32.DLL"
FUNCTION Long seeVerifyFormat (String EmailAddr) LIBRARY "SEE32.DLL"
FUNCTION Long seeVerifyUser (Long Chan, String EmailAddr) LIBRARY "SEE32.DLL"
end prototypes

type variables
Constant	Date		id_FechaLiberacion	= Today()
Constant	Time		it_HoraLiberacion		= Now()
Private Constant	String	_URL = 'https://rioblanco-api-certificacion.azurewebsites.net/'
end variables

forward prototypes
public subroutine wf_cargacertificaciones ()
public function boolean wf_protocolos ()
public function boolean wf_grabar (datastore ds_1)
public function boolean wf_certificadoras ()
public function boolean wf_categorias ()
public function boolean wf_estados ()
public function boolean wf_certificadoproductor ()
end prototypes

public subroutine wf_cargacertificaciones ();Integer li_Respuesta 

li_Respuesta = MessageBox('Atencion', 'Se procedera a actulizar informacion certificaciones.~n~nDesea Continuar?', Information!, YesNo!, 2)

If li_Respuesta = 1 Then
	SetPointer(HourGlass!)
	
	wf_Protocolos()
	wf_Categorias()
	wf_Certificadoras()
	wf_Estados()
	wf_CertificadoProductor()
	
	 MessageBox('Atencion', 'Proceso de carga Terminado', Information!, OK!)
End If

SetPointer(Arrow!)
Return
end subroutine

public function boolean wf_protocolos ();Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila
DataStore	ds_1, ds_2

ds_1	= Create DataStore
ds_2	= Create DataStore

lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

ds_2.DataObject = 'dw_mues_protocolos'
ls_URL = _URL + 'api/Protocolos/Retrieve' 
lnv_RestClient.Retrieve(ds_2, ls_URL)

If ds_2.RowCount() > 0 Then
	
	ds_1.DataObject = 'dw_mues_protocolos'
	ds_1.SetTransObject(SQLCA)
	ds_1.Retrieve()
	
	For ll_Fila = 1 To ds_2.RowCount()		  
		ls_Busca = 'prot_codigo = '  + String(ds_2.Object.prot_Codigo[ll_Fila])
		
		ll_Busca = ds_1.Find(ls_Busca, 1, ds_1.RowCount(), Primary!)
	
		If ll_Busca = 0 Then
			ds_2.RowsCopy(ll_Fila, ll_Fila, Primary!, ds_1, 1, Primary!)
		Else	 
			ds_1.Object.prot_Nombre[ll_Busca]	= ds_2.Object.prot_Nombre[ll_Fila]
			ds_1.Object.prot_Abrevi[ll_Busca]		= ds_2.Object.prot_Abrevi[ll_Fila]
			ds_1.Object.prot_Nroggn[ll_Busca]	= ds_2.Object.prot_Nroggn[ll_Fila]
		End If
	Next
	wf_Grabar(ds_1) 
End If

Destroy ds_1
Destroy ds_2
Destroy lnv_RestClient

Return lb_Retorno
end function

public function boolean wf_grabar (datastore ds_1);Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If ds_1.Update(True, False) = 1 Then
	Commit;
	If sqlca.SQLCode <> 0 Then
		F_ErrorBaseDatos(sqlca, 'Carga de Certificaciones')
		RollBack;
	Else
		lb_Retorno	=	True
		ds_1.ResetUpdate()
	End If
Else
	F_ErrorBaseDatos(sqlca, 'Carga de Certificaciones')
	RollBack;
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public function boolean wf_certificadoras ();Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila
DataStore	ds_1, ds_2

ds_1	= Create DataStore
ds_2	= Create DataStore

lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

ds_2.DataObject = 'dw_mues_certificadoras'
ls_URL = _URL + 'api/Certificadoras/Retrieve' 
lnv_RestClient.Retrieve(ds_2, ls_URL)

If ds_2.RowCount() > 0 Then
	
	ds_1.DataObject = 'dw_mues_certificadoras'
	ds_1.SetTransObject(SQLCA)
	ds_1.Retrieve()
	
	For ll_Fila = 1 To ds_2.RowCount()		  
		ls_Busca = 'cert_codigo = '  + String(ds_2.Object.cert_Codigo[ll_Fila])
		
		ll_Busca = ds_1.Find(ls_Busca, 1, ds_1.RowCount(), Primary!)
	
		If ll_Busca = 0 Then
			ds_2.RowsCopy(ll_Fila, ll_Fila, Primary!, ds_1, 1, Primary!)
		Else	 
			ds_1.Object.cert_Nombre[ll_Busca]	= ds_2.Object.cert_Nombre[ll_Fila]
			ds_1.Object.cert_Abrevi[ll_Busca]		= ds_2.Object.cert_Abrevi[ll_Fila]
		End If
	Next
	wf_Grabar(ds_1) 
End If

Destroy ds_1
Destroy ds_2
Destroy lnv_RestClient

Return lb_Retorno
end function

public function boolean wf_categorias ();Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila
DataStore	ds_1, ds_2

ds_1	= Create DataStore
ds_2	= Create DataStore

lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

ds_2.DataObject = 'dw_mues_categorias_cert'
ls_URL = _URL + 'api/Categorias/Retrieve' 
lnv_RestClient.Retrieve(ds_2, ls_URL)

If ds_2.RowCount() > 0 Then
	
	ds_1.DataObject = 'dw_mues_categorias_cert'
	ds_1.SetTransObject(SQLCA)
	ds_1.Retrieve()
	
	For ll_Fila = 1 To ds_2.RowCount()		  
		ls_Busca = 'cace_codigo = '  + String(ds_2.Object.cace_Codigo[ll_Fila])
		
		ll_Busca = ds_1.Find(ls_Busca, 1, ds_1.RowCount(), Primary!)
	
		If ll_Busca = 0 Then
			ds_2.RowsCopy(ll_Fila, ll_Fila, Primary!, ds_1, 1, Primary!)
		Else	 
			ds_1.Object.cace_Nombre[ll_Busca]	= ds_2.Object.cace_Nombre[ll_Fila]
			ds_1.Object.cace_Abrevi[ll_Busca]		= ds_2.Object.cace_Abrevi[ll_Fila]
		End If
	Next
	wf_Grabar(ds_1) 
End If

Destroy ds_1
Destroy ds_2
Destroy lnv_RestClient

Return lb_Retorno
end function

public function boolean wf_estados ();Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila
DataStore	ds_1, ds_2

ds_1	= Create DataStore
ds_2	= Create DataStore

lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

ds_2.DataObject = 'dw_mues_estadocertific'	
ls_URL = _URL + 'api/Estado/Retrieve' 
lnv_RestClient.Retrieve(ds_2, ls_URL)

If ds_2.RowCount() > 0 Then
	
	ds_1.DataObject = 'dw_mues_estadocertific'
	ds_1.SetTransObject(SQLCA)
	ds_1.Retrieve()
	
	For ll_Fila = 1 To ds_2.RowCount()		  
		ls_Busca = 'prec_codigo = '  + String(ds_2.Object.prec_Codigo[ll_Fila])
		
		ll_Busca = ds_1.Find(ls_Busca, 1, ds_1.RowCount(), Primary!)
	
		If ll_Busca = 0 Then
			ds_2.RowsCopy(ll_Fila, ll_Fila, Primary!, ds_1, 1, Primary!)
		Else	 
			ds_1.Object.prec_Nombre[ll_Busca]	= ds_2.Object.prec_Nombre[ll_Fila]
			ds_1.Object.prec_Abrevi[ll_Busca]		= ds_2.Object.prec_Abrevi[ll_Fila]
		End If
	Next
	wf_Grabar(ds_1) 
End If

Destroy ds_1
Destroy ds_2
Destroy lnv_RestClient

Return lb_Retorno
end function

public function boolean wf_certificadoproductor ();Boolean 		lb_Retorno = True
RESTClient	lnv_RestClient
String 		ls_URL, ls_Busca
Long			ll_Busca, ll_New, ll_Fila
DataStore	ds_1, ds_2

ds_1	= Create DataStore
ds_2	= Create DataStore

lnv_RestClient	=	Create RESTClient
lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")

ds_2.DataObject = 'dw_mues_certificacionprod'	
ls_URL = _URL + 'api/CertificacionProd/Retrieve' 
lnv_RestClient.Retrieve(ds_2, ls_URL)

If ds_2.RowCount() > 0 Then
	
	ds_1.DataObject = 'dw_mues_certificacionprod'
	ds_1.SetTransObject(SQLCA)
	ds_1.Retrieve()
	
	For ll_Fila = 1 To ds_2.RowCount()		  
		ls_Busca = 'prod_Codigo = '  + String(ds_2.Object.prod_Codigo[ll_Fila]) + ' And prpr_Codigo = ' + String(ds_2.Object.prpr_Codigo[ll_Fila]) + &
						' and espe_Codigo = '  + String(ds_2.Object.espe_Codigo[ll_Fila]) + ' And prot_Codigo = ' + String(ds_2.Object.prot_Codigo[ll_Fila])
		ll_Busca = ds_1.Find(ls_Busca, 1, ds_1.RowCount(), Primary!)
	
		If ll_Busca = 0 Then
			ds_2.RowsCopy(ll_Fila, ll_Fila, Primary!, ds_1, 1, Primary!)
		Else	 
			ds_1.Object.cert_Codigo[ll_Busca]		= ds_2.Object.cert_Codigo[ll_Fila]
			ds_1.Object.cace_Codigo[ll_Busca]	= ds_2.Object.cace_Codigo[ll_Fila]			
			ds_1.Object.prec_Codigo[ll_Busca]	= ds_2.Object.prec_Codigo[ll_Fila]
			ds_1.Object.cece_Nroins[ll_Busca]		= ds_2.Object.cece_Nroins[ll_Fila]
			ds_1.Object.cece_Ggngap[ll_Busca]	= ds_2.Object.cece_Ggngap[ll_Fila]
			ds_1.Object.cece_Fecaud[ll_Busca]	= ds_2.Object.cece_Fecaud[ll_Fila]
			ds_1.Object.cece_Fecexp[ll_Busca]	= ds_2.Object.cece_Fecexp[ll_Fila]
			ds_1.Object.cece_Fecins[ll_Busca]		= ds_2.Object.cece_Fecins[ll_Fila]
			ds_1.Object.cece_Feesau[ll_Busca]	= ds_2.Object.cece_Feesau[ll_Fila]
			ds_1.Object.cece_Montos[ll_Busca]	= ds_2.Object.cece_Montos[ll_Fila]
			ds_1.Object.cece_Observ[ll_Busca]	= ds_2.Object.cece_Observ[ll_Fila]
			ds_1.Object.cece_Rutas[ll_Busca]		= ds_2.Object.cece_Rutas[ll_Fila]
			ds_1.Object.cert_Codigo[ll_Busca]		= ds_2.Object.cert_Codigo[ll_Fila]
			ds_1.Object.cece_Archiv[ll_Busca]		= ds_2.Object.cece_Archiv[ll_Fila]
			ds_1.Object.cece_Recert[ll_Busca]	= ds_2.Object.cece_Recert[ll_Fila]
			ds_1.Object.cece_Packin[ll_Busca]		= ds_2.Object.cece_Packin[ll_Fila]
		End If
	Next
	wf_Grabar(ds_1) 
End If

Destroy ds_1
Destroy ds_2
Destroy lnv_RestClient

Return lb_Retorno
end function

on certificaciones.create
appname="certificaciones"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on certificaciones.destroy
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
gstr_apl.Titulo				=	"SISTEMA DE CERTIFICACIONES"
gstr_apl.Ini					=	"Certificaciones.ini"
gstr_apl.Bmp				=	"\Desarrollo 17\Imagenes\Sistemas\Certificaciones.jpg"
gstr_apl.Icono				=	"\Desarrollo 17\Imagenes\Sistemas\Certificaciones.ico"
gstr_apl.liberacion			=	F_Fecha_Carta(id_FechaLiberacion, 3) + "  " + &
									String(it_HoraLiberacion)
gstr_apl.version			=	"5.22.28022023"
gstr_apl.fechalibera		=  id_FechaLiberacion
gstr_apl.CodigoSistema	=	22
gstr_apl.NombreSistema	=	"Certificaciones"

String ls_parametros
ls_parametros				=	CommandParm()
OpenWithParm(w_acceso, ls_parametros)

IF Message.DoubleParm <> 1 THEN
	HALT
	RETURN
END IF

IF AccesoSistemaValido() THEN
	ParEmpresa()
	If gstr_parempresa.Certificacion = 1 Then wf_CargaCertificaciones()
	
	Open(w_main)
ELSE
	HALT
	RETURN
END IF
end event

