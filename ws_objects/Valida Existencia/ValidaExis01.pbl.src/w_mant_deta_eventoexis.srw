$PBExportHeader$w_mant_deta_eventoexis.srw
forward
global type w_mant_deta_eventoexis from w_mant_detalle
end type
end forward

global type w_mant_deta_eventoexis from w_mant_detalle
integer width = 2103
integer height = 1188
end type
global w_mant_deta_eventoexis w_mant_deta_eventoexis

forward prototypes
public function boolean duplicado (string campo, integer tipo)
end prototypes

public function boolean duplicado (string campo, integer tipo);Long		ll_fila
String		ls_codigo, ls_Tipo

ls_Tipo	= String(dw_1.GetItemNumber(il_fila,"evex_tipova"))
ls_codigo	= String(dw_1.GetItemNumber(il_fila,"evex_codigo"))

ls_Tipo 	= String(Tipo)
ls_codigo	= campo

ll_fila	= istr_mant.dw.Find("evex_tipova = " + ls_Tipo + " And evex_codigo = " + ls_codigo, 1, istr_mant.dw.RowCount())

If ll_fila > 0 And ll_fila <> il_FilaAnc Then 
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	Return True
Else
	Return False
End If
end function

on w_mant_deta_eventoexis.create
call super::create
end on

on w_mant_deta_eventoexis.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemNumber(il_fila, "evex_tipova"))
ias_campo[2] = String(dw_1.GetItemNumber(il_fila, "evex_codigo"))
ias_campo[3] = dw_1.GetItemString(il_fila, "evex_nombre")


If Not  istr_mant.Agrega And Not istr_mant.Borra Then 
	dw_1.Object.evex_tipova.Protect = 1
	dw_1.Object.evex_codigo.Protect = 1
	dw_1.Object.evex_tipova.BackGround.Color	= 553648127
	dw_1.Object.evex_codigo.BackGround.Color	= 553648127
	dw_1.Object.evex_tipova.Color					= RGB(255,255,255)
	dw_1.Object.evex_codigo.Color				= RGB(255,255,255)
END IF

end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.SetItem(il_fila, "evex_tipova", Long(ias_campo[1]))
	dw_1.SetItem(il_fila, "evex_codigo", Long(ias_campo[2]))
	dw_1.SetItem(il_fila, "evex_nombre", ias_campo[3])
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF IsNull(dw_1.GetItemNumber(il_fila, "evex_tipova")) OR dw_1.GetItemNumber(il_fila, "evex_tipova") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTipo Evento de Validacion de Existencia"
	ls_colu[li_cont]	= "evex_tipova"
END IF

IF IsNull(dw_1.GetItemNumber(il_fila, "evex_codigo")) OR dw_1.GetItemNumber(il_fila, "evex_codigo") = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Evento de Validacion de Existencia"
	ls_colu[li_cont]	= "evex_codigo"
END IF

IF IsNull(dw_1.GetItemString(il_fila, "evex_nombre")) THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNombre de Evento de Validacion de Existencia"
	ls_colu[li_cont]	= "evex_nombre"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_eventoexis
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_eventoexis
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_eventoexis
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_eventoexis
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_eventoexis
integer x = 1687
integer y = 464
integer taborder = 20
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_eventoexis
integer x = 1687
integer y = 280
integer taborder = 10
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_eventoexis
integer x = 1696
integer y = 640
integer taborder = 30
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_eventoexis
integer width = 1381
integer height = 812
integer taborder = 0
string dataobject = "dw_mant_eventoexis"
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_Null
String		ls_campo

ls_campo =dwo.Name

SetNull(ll_null)

Choose Case ls_campo
	Case "evex_tipova"
		If Duplicado(String(This.Object.evex_codigo[Row]), Integer(Data)) Then
			dw_1.SetItem(il_fila, ls_campo, ll_null)
			Return 1
		End If
		
	Case "evex_codigo"
		If Duplicado(Data, This.Object.evex_tipova[Row]) Then
			dw_1.SetItem(il_fila, ls_campo, ll_null)
			Return 1
		End If
		
End Choose 
end event

