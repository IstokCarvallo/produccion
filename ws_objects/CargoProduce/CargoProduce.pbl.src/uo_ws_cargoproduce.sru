$PBExportHeader$uo_ws_cargoproduce.sru
forward
global type uo_ws_cargoproduce from nonvisualobject
end type
end forward

global type uo_ws_cargoproduce from nonvisualobject
end type
global uo_ws_cargoproduce uo_ws_cargoproduce

type variables
Private uo_Mail		iuo_Correo
Private DataStore	ids_JSON, ids_SKU, ids_Contenedor

Private 	String		_URL
Private 	String		_HEADERS
Private 	String		_ASUNTO
Private 	String		_CORREO
Private 	String		_CUERPO

Private Constant	String	  	_INICIO			= '{'
Private Constant	String	  	_FINAL			= '}'
Private Constant	String	 	_INICIOBLOQUE= '['
Private Constant	String	 	_FINALBLOQUE	= ']'
Private Constant	String	 	_SEPARADOR	= ':'
Private Constant	String	 	_MARCA			= '"'
Private Constant	String	 	_FINLINEA		= ','
Private Constant	String	 	_USER 			= 'istok.carvallo@rioblanco.net'
Private Constant	String	 	_PASSWORD	= "10bh0q10*"

end variables

forward prototypes
public subroutine of_setheaders (string codigo)
public subroutine of_seturl (string codigo)
public subroutine of_resultado (string as_json, ref datawindow adw)
public function integer of_enviacorreo ()
public subroutine of_setasunto (string codigo)
public subroutine of_setcorreo (string codigo)
public subroutine of_setcuerpo (string codigo)
private function string of_cargajson (string embarque)
private function string of_insertalinea (string tag, string valor, byte final, byte numero)
private function string of_cargamulti (string embarque)
private function string of_obtienetoken ()
public function integer of_carga (string embarque, ref string respuesta)
end prototypes

public subroutine of_setheaders (string codigo);If IsNull(Codigo) Then Codigo = ''
This._Headers	= Trim(Codigo)
end subroutine

public subroutine of_seturl (string codigo);If IsNull(Codigo) Then Codigo = ''
This._URL	= Trim(Codigo)
end subroutine

public subroutine of_resultado (string as_json, ref datawindow adw);Long	ll_RowCount
String ls_Error

adw.Reset()

//{"message":"Carga realizada con exito."}

ll_RowCount = adw.ImportJsonByKey(as_Json, ls_Error, Primary!)

If IsNull(ll_RowCount) Then
	Messagebox("Error", "El metodo retorno null")
ElseIf ll_RowCount < 0 Then
	If Len(ls_Error) > 0 Then
		Messagebox("Fallo...","Valor Retorno: " + String(ll_RowCount) + "~r~nCon informacion de Error:~r~n" + ls_Error)
	 Else
  		Messagebox("Fallo...","Valor Retorno: "+String(ll_RowCount))
	 End If
Else
	//Checks if any warning
 	If len(ls_Error) > 0 Then
  		MessageBox("Alerta", "Con Informaion de Alerta:~r~n" + ls_Error)
	 Else
//		MessageBox("Correcto", "Valor Retorno: " + String(ll_RowCount) )
	 End If
End If

of_SetCuerpo(as_Json)
of_EnviaCorreo()

end subroutine

public function integer of_enviacorreo ();String		ls_Error, ls_Correo[]
Integer	li_Respuesta

//li_Respuesta =  iuo_Correo.Send_Mail("192.168.150.245","<SendMail@rioblanco.net>", _Correo, "", "", _Asunto, _Cuerpo, "", ls_Error)
//li_Respuesta =  iuo_Correo.Send_Mail("192.168.150.245","<SendMail@rioblanco.net>", "<istok.carvallo@rioblanco.net>", "", "", _Asunto, _Cuerpo, "", ls_Error)
ls_correo = {"<istok.carvallo@rioblanco.net>"}
	
iuo_Correo.of_Send(ls_Correo, _Asunto, _Cuerpo,0)

//If (li_Respuesta < 0) Then MessageBox("Error No" + string(li_Respuesta), ls_Error)

Return li_Respuesta
end function

public subroutine of_setasunto (string codigo);If IsNull(Codigo) Then Codigo = ''
This._Asunto = Trim(Codigo)
end subroutine

public subroutine of_setcorreo (string codigo);If IsNull(Codigo) Then Codigo = ''
This._Correo = Trim(Codigo)
end subroutine

public subroutine of_setcuerpo (string codigo);If IsNull(Codigo) Then Codigo = ''
This._Cuerpo	= Trim(Codigo)
end subroutine

private function string of_cargajson (string embarque);/*
Funcion Obsoleta para carga de archivos CargoProduce
*/
String	ls_Retorno
Long	ll_Fila, Row, RowD

ll_Fila = ids_JSON.Retrieve(Embarque, 0, 1) 
ll_Fila = ids_SKU.Retrieve(Embarque, 1, 0) 

If ll_Fila = -1 Then
	MessageBox('Atencion', 'Error de carga de store....', StopSign!, Ok!)
	ls_Retorno = '*'
ElseIf ll_Fila = 0 Then
	MessageBox('Atencion', 'No se encontro informacion....', StopSign!, Ok!)
	ls_Retorno = '*'
Else
	ls_Retorno = _INICIO  + Char(13)
	ls_Retorno += of_InsertaLinea('poNumber', String(ids_Json.Object.poNumber[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('referenceNumber', String(ids_Json.Object.referenceNumber[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('exporterName', String(ids_Json.Object.exporterName[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('importerName', String(ids_Json.Object.importerName[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('transportMode', 'Ocean', 0, 0)
	ls_Retorno += of_InsertaLinea('portOfOriginCode', String(ids_Json.Object.portOfOriginCode[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('portOfDestinationCode', String(ids_Json.Object.portOfDestinationCode[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('containerIdentifier' , String(ids_Json.Object.containerIdentifier[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('bookingNumber', String(ids_Json.Object.bookingNumber[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('billOfLading', '', 0, 0)
	ls_Retorno += of_InsertaLinea('oceanCarrierCode', String(ids_Json.Object.oceanCarrierCode[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('etd', String(ids_Json.Object.etd[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('eta', String(ids_Json.Object.eta[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('isReefer', String(ids_Json.Object.isreefer[1]), 0, 1)
	ls_Retorno += of_InsertaLinea('vesselName', String(ids_Json.Object.referenceNumber[1]), 0, 0)
	
	ls_Retorno += _MARCA + 'poLines' + _MARCA  + _Separador + _INICIOBLOQUE 
	For RowD = 1 To ids_SKU.RowCount()
		ls_Retorno += _INICIO + Char(13)
		
		//ls_Retorno += of_InsertaLinea('currency', '', 0, 0)
		ls_Retorno += of_InsertaLinea('pricePerBox', '0', 0, 1)
		ls_Retorno += of_InsertaLinea('pallets', String(ids_SKU.Object.pallet[RowD]), 0, 1)
		ls_Retorno += of_InsertaLinea('boxes', String(ids_SKU.Object.cajas[RowD]), 0, 1)
		ls_Retorno += of_InsertaLinea('weight', String(ids_SKU.Object.kilos[RowD]), 0, 1)
		ls_Retorno += of_InsertaLinea('pricePerMeasurementUnit', '0', 0, 1)
		ls_Retorno += of_InsertaLinea('measurementUnitCode', 'Kg', 0, 0)
		ls_Retorno += of_InsertaLinea('skuName', String(ids_SKU.Object.SKU[RowD]), 1, 0)
		If RowD = ids_SKU.RowCount() Then
			ls_Retorno += _FINAL + Char(13)
		Else
			ls_Retorno += _FINAL + Char(13) + _FINLINEA + Char(13)
		End If
	Next
	ls_Retorno += _FINALBLOQUE + _FINLINEA + Char(13)
	
	ls_Retorno += _MARCA + 'packingListLines' + _MARCA  + _Separador + _INICIOBLOQUE + _FINALBLOQUE + _FINLINEA + Char(13)
	ls_Retorno += _MARCA + 'tagNames' + _MARCA  + _Separador + _INICIOBLOQUE + _FINALBLOQUE + _FINLINEA + Char(13)
	ls_Retorno += _MARCA + 'customFields' + _MARCA  + _Separador + _INICIOBLOQUE + Char(13)	
	ls_Retorno += _FINALBLOQUE + Char(13) + _FINAL + Char(13)
End If

Return ls_Retorno
end function

private function string of_insertalinea (string tag, string valor, byte final, byte numero);String	ls_Retorno

If IsNull(TAG) Then TAG = ''
If IsNull(Valor) Then Valor = ''

If Numero = 1 Then Valor = F_Global_Replace(Valor, ',', '.')

If Final = 1 Then
	If Numero = 1 Then
		ls_Retorno = _MARCA + TAG + _MARCA + _Separador + Valor + Char(13)
	Else
		ls_Retorno = _MARCA + TAG + _MARCA + _Separador + _MARCA + Valor + _MARCA + Char(13)
	End If
Else
	If Numero = 1 Then
		ls_Retorno = _MARCA + TAG + _MARCA + _Separador + Valor + _FINLINEA + Char(13)
	Else
		ls_Retorno = _MARCA + TAG + _MARCA + _Separador + _MARCA + Valor + _MARCA + _FINLINEA + Char(13)
	End If
End If

Return ls_Retorno
end function

private function string of_cargamulti (string embarque);String	ls_Retorno
Long	ll_Fila, Row, RowD

ll_Fila = ids_JSON.Retrieve(Embarque, 0, 0) 
ll_Fila = ids_SKU.Retrieve(Embarque, 1, 0) 
ll_Fila = ids_Contenedor.Retrieve(Embarque, 0, 1) 

If ll_Fila = -1 Then
	MessageBox('Atencion', 'Error de carga de store....', StopSign!, Ok!)
	ls_Retorno = '*'
ElseIf ll_Fila = 0 Then
	MessageBox('Atencion', 'No se encontro informacion....', StopSign!, Ok!)
	ls_Retorno = '*'
Else
	ls_Retorno = _INICIO  + Char(13)
	ls_Retorno += of_InsertaLinea('poNumber', String(ids_Json.Object.poNumber[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('referenceNumber', String(ids_Json.Object.referenceNumber[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('exporterName', String(ids_Json.Object.exporterName[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('importerName', String(ids_Json.Object.importerName[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('transportMode', 'Ocean', 0, 0)
	ls_Retorno += of_InsertaLinea('portOfOriginCode', String(ids_Json.Object.portOfOriginCode[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('portOfDestinationCode', String(ids_Json.Object.portOfDestinationCode[1]), 0, 0)
	ls_Retorno += of_InsertaLinea('billOfLading', '', 0, 0)
	
	ls_Retorno += _MARCA + 'shipments' + _MARCA  + _Separador + _INICIOBLOQUE 
	For RowD = 1 To ids_Contenedor.RowCount()
		ls_Retorno += _INICIO + Char(13)
		ls_Retorno += of_InsertaLinea('oceanCarrierCode', String(ids_Contenedor.Object.oceanCarrierCode[RowD]), 0, 0)
		ls_Retorno += of_InsertaLinea('containerIdentifier' , String(ids_Contenedor.Object.containerIdentifier[RowD]), 0, 0)
		ls_Retorno += of_InsertaLinea('etd', String(ids_Contenedor.Object.etd[RowD]), 0, 0)
		ls_Retorno += of_InsertaLinea('eta', String(ids_Contenedor.Object.eta[RowD]), 0, 0)
		ls_Retorno += of_InsertaLinea('isReefer', String(ids_Contenedor.Object.isreefer[RowD]), 0, 1)
		ls_Retorno += of_InsertaLinea('vesselName', String(ids_Contenedor.Object.referenceNumber[RowD]), 0, 0)
		ls_Retorno += of_InsertaLinea('bookingNumber', String(ids_Contenedor.Object.bookingNumber[RowD]), 1, 0)
		If RowD = ids_Contenedor.RowCount() Then
			ls_Retorno += _FINAL + Char(13)
		Else
			ls_Retorno += _FINAL + Char(13) + _FINLINEA + Char(13)
		End If
	Next
	ls_Retorno += _FINALBLOQUE + _FINLINEA + Char(13)
	
	ls_Retorno += _MARCA + 'poLines' + _MARCA  + _Separador + _INICIOBLOQUE 
	For RowD = 1 To ids_SKU.RowCount()
		ls_Retorno += _INICIO + Char(13)
		
		//ls_Retorno += of_InsertaLinea('currency', '', 0, 0)
		ls_Retorno += of_InsertaLinea('pricePerBox', '0', 0, 1)
		ls_Retorno += of_InsertaLinea('pallets', String(ids_SKU.Object.pallet[RowD]), 0, 1)
		ls_Retorno += of_InsertaLinea('boxes', String(ids_SKU.Object.cajas[RowD]), 0, 1)
		ls_Retorno += of_InsertaLinea('weight', String(ids_SKU.Object.kilos[RowD]), 0, 1)
		ls_Retorno += of_InsertaLinea('pricePerMeasurementUnit', '0', 0, 1)
		ls_Retorno += of_InsertaLinea('measurementUnitCode', 'Kg', 0, 0)
		ls_Retorno += of_InsertaLinea('skuName', String(ids_SKU.Object.SKU[RowD]), 1, 0)
		If RowD = ids_SKU.RowCount() Then
			ls_Retorno += _FINAL + Char(13)
		Else
			ls_Retorno += _FINAL + Char(13) + _FINLINEA + Char(13)
		End If
	Next
	ls_Retorno += _FINALBLOQUE + _FINLINEA + Char(13)
	
	ls_Retorno += _MARCA + 'packingListLines' + _MARCA  + _Separador + _INICIOBLOQUE + _FINALBLOQUE + _FINLINEA + Char(13)
	ls_Retorno += _MARCA + 'tagNames' + _MARCA  + _Separador + _INICIOBLOQUE + _FINALBLOQUE + _FINLINEA + Char(13)
	ls_Retorno += _MARCA + 'customFields' + _MARCA  + _Separador + _INICIOBLOQUE + Char(13)	
	ls_Retorno += _FINALBLOQUE + Char(13) + _FINAL + Char(13)
End If

Return ls_Retorno
end function

private function string of_obtienetoken ();String				ls_Responsebody, ls_PostData, ls_Retorno
Integer			li_SendReturn, ll_Fila
RestClient 		lrc_WSRossi
JsonPackage	ljp_Token

_URL			=	"https://api.cargoproduce.com/api/Account/Login"
_HEADERS	= "Content-Type: application/json"

lrc_WSRossi = Create RestClient 
ljp_Token	=	Create JsonPackage
lrc_WSRossi.SetRequestHeaders(_HEADERS)

ls_PostData = _INICIO  + Char(13)
ls_PostData += of_InsertaLinea('userName', _USER, 0, 0)
ls_PostData += of_InsertaLinea('password', _PASSWORD, 1, 0)
ls_PostData += _FINAL + Char(13)

li_SendReturn = lrc_WSRossi.SendPostRequest(_URL, ls_PostData, ls_Responsebody)

If li_SendReturn <> 1 Or lrc_WSRossi.GetResponseStatusCode() <> 200 Then
//	MessageBox('Atencion', 'No se pudo conectar a WEB SERVICES...', StopSign!, Ok!)
	of_SetCuerpo('No se pudo conectar a WEB SERVICES...')
	of_EnviaCorreo()
	ls_Retorno = '*'
Else
	 ljp_Token.LoadString(ls_Responsebody)
	 ls_Retorno = ljp_Token.GetValue("token")
End If

If IsValid(lrc_WSRossi) Then Destroy lrc_WSRossi

Return ls_Retorno
end function

public function integer of_carga (string embarque, ref string respuesta);String			ls_Responsebody, ls_PostData, ls_Token
Integer		li_SendReturn, li_Retorno = 1, ll_Fila
RestClient 	lrc_WS

ls_Token = of_ObtieneToken()

if ls_Token <> '*' Then 
	_HEADERS = 'Authorization: Bearer ' + ls_Token
	_URL = "https://api.cargoproduce.com/api/ExternalPos/Grouped/Upsert"
	
	If li_Retorno <> -1 Then 
		lrc_WS = Create RestClient 
		lrc_WS.SetRequestHeaders(_HEADERS)
		_HEADERS = "Content-Type: application/json"
		lrc_WS.SetRequestHeaders(_HEADERS)
		
		ls_PostData = of_CargaMulti(Embarque) 
		
		If ls_PostData <> '*' Then
			li_SendReturn = lrc_WS.SendPostRequest(_URL, ls_PostData, ls_Responsebody)
			If li_SendReturn <> 1 Or lrc_WS.GetResponseStatusCode() <> 200 Then
	//			MessageBox('Atencion', ls_Responsebody + ' Problemas en actualizacion de Embarque: ' + Embarque, StopSign!, Ok!)
				respuesta = ls_Responsebody
				of_SetCuerpo('No se pudo conectar a Servicio Web, Actualizacion de Embarque (' + Embarque+ ')...~r~n~r~n' + ls_Responsebody + &
							 '~r~n~r~nArchivo de Carga...~r~n~r~n~r~n~r~n' + ls_PostData)
				of_SetAsunto('Carga PO: ' + Embarque + ' / CargProduce...')
				of_EnviaCorreo()
				li_Retorno = -1
			End If
		Else
			li_Retorno = -1
		End If
		If IsValid(lrc_WS) Then Destroy lrc_WS
	End If
Else
	li_Retorno = -1
End If

Return li_Retorno
end function

event constructor;ids_JSON			=	Create DataStore
ids_SKU			=	Create DataStore
ids_Contenedor	=	Create DataStore

iuo_Correo		=	Create uo_Mail

ids_JSON.DataObject = 'dw_gene_json_embq'
ids_JSON.SetTransObject(Sqlca)

ids_SKU.DataObject = 'dw_gene_json_embq'
ids_SKU.SetTransObject(Sqlca)

ids_Contenedor.DataObject = 'dw_gene_json_embq'
ids_Contenedor.SetTransObject(Sqlca)

end event

on uo_ws_cargoproduce.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_ws_cargoproduce.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event destructor;Destroy ids_JSON
Destroy ids_SKU
Destroy ids_Contenedor
Destroy iuo_Correo
end event

