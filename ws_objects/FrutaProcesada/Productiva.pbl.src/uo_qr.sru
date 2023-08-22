$PBExportHeader$uo_qr.sru
forward
global type uo_qr from nonvisualobject
end type
end forward

global type uo_qr from nonvisualobject
end type
global uo_qr uo_qr

type variables
OleObject	Qrcode
String			is_Ruta
end variables

forward prototypes
public function string of_genera_qr (string as_data)
end prototypes

public function string of_genera_qr (string as_data);int 	li_Code
String ls_Ruta,  ls_Data,  ls_Tipo, ls_Path

ls_Path = GetCurrentDirectory()

If Not DirectoryExists(ls_Path+"\QR_IMAGEN" ) Then CreateDirectory(ls_Path+"\QR_IMAGEN" )

Qrcode = CREATE OleObject

li_Code = Qrcode.ConnectToNewObject("Facturador.Firma")

If li_Code <> 0 Then
	ls_Ruta = ''
Else
	ls_Ruta = ls_Path+"\QR_IMAGEN\" 
	ls_data = as_data
	ls_tipo = "PNG"
	
	ls_Ruta = Qrcode.codebarqr(ls_Ruta, ls_data, ls_tipo )

	Qrcode.DisconnectObject()
End If

Destroy Qrcode

GarbageCollect ( ) 

Return ls_Ruta
end function

on uo_qr.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_qr.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

