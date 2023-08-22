$PBExportHeader$uo_mail.sru
$PBExportComments$Objecto Usuario que valida la Parametros
forward
global type uo_mail from nonvisualobject
end type
end forward

shared variables

end variables

global type uo_mail from nonvisualobject
end type
global uo_mail uo_mail

type prototypes
FUNCTION Integer		seeAbort (Long Chan) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeAttach (Long NbrChans, Long KeyCode) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeClose (Long Chan) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeCommand (Long Chan, String Text) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeDebug (Long Chan, Long Index, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeDecodeBuffer (String CodedBuf, String ClearBuf, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeDecodeUTF8 (String UTF8Buffer, String UnicodeBuffer) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeDecodeUU (String CodedBuf, String ClearBuf) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeDeleteEmail (Long Chan, Long MsgNbr) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeDriver (Long Chan) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeEncodeBuffer (String ClearBuf, String CodedBuf, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeEncodeUTF8 (Long UnicodeValue, String UTF8Buffer) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeErrorText (Long Chan, Long Code, ref String Buffer, Long BufLen) LIBRARY "SEE32.DLL" alias for "seeErrorText;Ansi"
FUNCTION Long 		seeExtractLine (String Src, Long LineNbr, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeExtractText (String Src, String Text, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeGetEmailCount (Long Chan) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeGetEmailFile (Long Chan, Long MsgNbr, String FileName, String EmailDir, String AttachDir) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeGetEmailLines (Long Chan, Long MsgNbr, Long Lines, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeGetEmailSize (Long Chan, Long MsgNbr) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeGetEmailUID (Long Chan, Long MsgNbr, String Buffer, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long		seeIsConnected(Long Chan) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeIntegerParam (Long Chan, Long Index, Long Value) LIBRARY "SEE32.DLL"
FUNCTION Long 		seePop3Connect (Long Chan, String Server, String User, String Password) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeQuoteBuffer (String ClearBuf, String CodedBuf, Long BufLen) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeRelease () LIBRARY "SEE32.DLL"
FUNCTION Long 		seeSendEmail (Long Chan, String Rcpt, String CC, String BCC, String Subj, String Msg, String Attach) LIBRARY "SEE32.DLL" alias for "seeSendEmail;Ansi"
FUNCTION Long 		seeSendHTML (Long Chan, String Rcpt, String CC, String BCC, String Subj, String Msg, String Images, String AltTxt, String Attach) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeSmtpConnect (Long Chan, String Server, String Fromm, String Reply) LIBRARY "SEE32.DLL" alias for "seeSmtpConnect;Ansi"
FUNCTION Long 		seeStatistics (Long Chan, Long Index) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeStringParam (Long Chan, Long Index, String Value) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeVerifyFormat (String EmailAddr) LIBRARY "SEE32.DLL"
FUNCTION Long 		seeVerifyUser (Long Chan, String EmailAddr) LIBRARY "SEE32.DLL"
end prototypes

type variables
String	SMTP

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

forward prototypes
public function long send_mail (string smtp_server, string email_from, string email_to, string email_cc, string email_bcc, string email_subject, string email_msg, string email_attach, ref string error_msg)
public function long send_mailhtml (string smtp_server, string email_from, string email_to, string email_cc, string email_bcc, string email_subject, string email_msg, string email_attach, ref string error_msg)
public subroutine of_setsmtp (string codigo)
public function integer of_obtienesmtp ()
end prototypes

public function long send_mail (string smtp_server, string email_from, string email_to, string email_cc, string email_bcc, string email_subject, string email_msg, string email_attach, ref string error_msg);Long ll_nbr_chans, ll_channel, ll_result, ll_nresult, ll_ParamIndex, ll_ParamValue, ll_largo, ll_Retorno
String	ls_Null

SetNull(ls_Null)
 
ll_largo 			= 	100
error_msg 		= 	space(ll_largo) + char(0)
ll_nbr_chans 	= 	1
ll_channel 		= 	0
seeClose (ll_channel)
seeRelease()

ll_result 			= 	seeAttach(ll_nbr_chans, SEE_KEY_CODE)

If ll_result < 0 Then
	ll_nresult = seeErrorText(ll_channel, ll_result, error_msg, ll_largo)
	error_msg = mid(error_msg,1,ll_nresult)
	seeClose (ll_channel)
	seeRelease()
	Return ll_result
End If

If (EMAIL_ATTACH = "") Then EMAIL_ATTACH = Char(0)

//Establecemos la Conexion
ll_result = seeSmtpConnect(ll_channel, smtp_server, email_from, email_from)
//ll_result = seeSmtpConnectSSL(0,8801,465,smtp_server,User,Pass,email_from,email_from,NULL)

If ll_result < 0 Then
	ll_nresult = seeErrorText(ll_channel, ll_result, error_msg, ll_largo)
	error_msg = mid(error_msg, 1, ll_nresult)
	seeClose (ll_channel)
	seeRelease()
	Return ll_result	
End If

ll_ParamIndex = SEE_QUOTED_PRINTABLE
ll_ParamValue = QUOTED_PLAIN
ll_result = seeIntegerParam(ll_channel, ll_ParamIndex, ll_ParamValue)

//Enviamos el Mail hasta el(los)destinatarios
ll_result = seeSendEmail(ll_channel, email_to, email_cc, email_bcc, email_subject, EMAIL_MSG, EMAIL_ATTACH) 

If ll_result < 0 Then
	ll_nresult = seeErrorText(ll_channel, ll_result, error_msg, ll_largo)
	seeClose (ll_channel)
	seeRelease()
	Return ll_result
End If

seeClose (ll_channel)
seeRelease()

Return 0
end function

public function long send_mailhtml (string smtp_server, string email_from, string email_to, string email_cc, string email_bcc, string email_subject, string email_msg, string email_attach, ref string error_msg);Long ll_nbr_chans, ll_channel, ll_result, ll_nresult, ll_ParamIndex, ll_ParamValue, ll_largo, ll_Retorno
String	ls_Null, EMAIL_IMAGES, EMAIL_ALTTEXT


SetNull(ls_Null)
 
ll_largo 			= 	100
error_msg 		= 	space(ll_largo) + char(0)
ll_nbr_chans 	= 	1
ll_channel 		= 	0
seeClose (ll_channel)
seeRelease()

ll_result 			= 	seeAttach(ll_nbr_chans, SEE_KEY_CODE)

If ll_result < 0 Then
	ll_nresult = seeErrorText(ll_channel, ll_result, error_msg, ll_largo)
	error_msg = mid(error_msg,1,ll_nresult)
	seeClose (ll_channel)
	seeRelease()
	Return ll_result
End If

If (EMAIL_ATTACH = "") Then EMAIL_ATTACH = Char(0)
If (EMAIL_IMAGES	= "") Then EMAIL_IMAGES = Char(0)
If (EMAIL_ALTTEXT = "") Then EMAIL_ALTTEXT = Char(0)

//Establecemos la Conexion
ll_result = seeSmtpConnect(ll_channel, smtp_server, email_from, email_from)

If ll_result < 0 Then
	ll_nresult = seeErrorText(ll_channel, ll_result, error_msg, ll_largo)
	error_msg = mid(error_msg, 1, ll_nresult)
	seeClose (ll_channel)
	seeRelease()
	Return ll_result	
End If

ll_ParamIndex = SEE_QUOTED_PRINTABLE
ll_ParamValue = QUOTED_PLAIN
ll_result = seeIntegerParam(ll_channel, ll_ParamIndex, ll_ParamValue)

//Enviamos el Mail hasta el(los)destinatarios
ll_result = seeSendHTML(ll_channel, email_to, email_cc, email_bcc, email_subject, EMAIL_MSG, EMAIL_IMAGES, EMAIL_ALTTEXT, EMAIL_ATTACH) 

If ll_result < 0 Then
	ll_nresult = seeErrorText(ll_channel, ll_result, error_msg, ll_largo)
	seeClose (ll_channel)
	seeRelease()
	Return ll_result
End If

seeClose (ll_channel)
seeRelease()

Return 0
end function

public subroutine of_setsmtp (string codigo);If IsNull(Codigo) Then Codigo = ''
This.SMTP	= Trim(Codigo)


end subroutine

public function integer of_obtienesmtp ();Integer	lb_Retorno = 0

If Not IsValid(gstr_ParEmpresa) Then 
	lb_Retorno = -1
Else
	If IsNull(gstr_ParEmpresa.SMTP) or gstr_ParEmpresa.SMTP = '' Then
		lb_Retorno = -2
	Else
		of_SetSMTP(gstr_ParEmpresa.SMTP)
	End If
End If

Return lb_Retorno
end function

on uo_mail.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_mail.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

