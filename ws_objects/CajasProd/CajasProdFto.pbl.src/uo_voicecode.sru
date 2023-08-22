$PBExportHeader$uo_voicecode.sru
forward
global type uo_voicecode from nonvisualobject
end type
end forward

global type uo_voicecode from nonvisualobject
end type
global uo_voicecode uo_voicecode

forward prototypes
public subroutine voicecode (datawindow dw, string as_emba_nro, string as_fecha, date ad_fecemb, integer ai_vopico)
end prototypes

public subroutine voicecode (datawindow dw, string as_emba_nro, string as_fecha, date ad_fecemb, integer ai_vopico);OLEObject varVoice
string ResVC
integer Return_Code

IF  ai_vopico = 1 THEN
	varVoice	   		= Create OLEObject 
	Return_Code    = varVoice.ConnectToNewObject("VoiceCodeLib.VoiceCode") 
						
	IF Return_Code <> 0 THEN
		 Destroy varVoice
		 Messagebox ("Error", "Error al utilizar la libreria "+STRING(RETURN_CODE))
		 Return 
	ELSE
		ResVC                           =   varVoice.Calcular(as_emba_nro, as_fecha, ad_fecemb) //(asGTIN, asLOTE, adFECHA)
		dw.Object.vp_1.Text		=	Mid(ResVC, 1, 2)
		dw.Object.vp_2.Text		=	Mid(ResVC, 3, 2)
		VarVoice.DisconnectObject()
		Destroy varVoice
	END IF
END IF
end subroutine

on uo_voicecode.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_voicecode.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

