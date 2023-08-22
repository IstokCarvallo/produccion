$PBExportHeader$w_mant_deta_ctlcalmadurez.srw
forward
global type w_mant_deta_ctlcalmadurez from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_ctlcalmadurez from w_mant_detalle_csd
integer width = 2231
integer height = 1192
string title = "NAVES"
end type
global w_mant_deta_ctlcalmadurez w_mant_deta_ctlcalmadurez

type variables

end variables

forward prototypes
public function boolean duplicado (string valor)
end prototypes

public function boolean duplicado (string valor);Long		ll_fila
Integer	li_codigo

li_codigo	=	Integer(valor)

ll_fila	= dw_1.Find("madu_codigo = " + String(li_codigo),1 , dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

on w_mant_deta_ctlcalmadurez.create
call super::create
end on

on w_mant_deta_ctlcalmadurez.destroy
call super::destroy
end on

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.SetItem(il_fila, "espe_codigo", 		Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "madu_codigo", 	Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "madu_nombre", 	ias_campo[3])
	dw_1.SetItem(il_fila, "madu_candig", 	Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "madu_valida", 		Integer(ias_campo[5]))
	dw_1.SetItem(il_fila, "madu_operad", 	Integer(ias_campo[6]))
	dw_1.SetItem(il_fila, "madu_inicio", 		Dec(ias_campo[7]))
	dw_1.SetItem(il_fila, "madu_termin", 	Dec(ias_campo[8]))
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

If Isnull(dw_1.GetItemNumber(il_fila, "madu_codigo")) OR dw_1.GetItemNumber(il_fila, "madu_codigo") = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCodigo"
	ls_colu[li_cont]	= "madu_codigo"
End If

If Isnull(dw_1.GetItemString(il_fila, "madu_nombre")) OR trim(dw_1.GetItemString(il_fila, "madu_nombre")) = "" Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNombre"
	ls_colu[li_cont]	= "madu_nombre"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.espe_codigo[il_Fila] = Integer(istr_mant.Argumento[1])
end event

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.espe_codigo[il_Fila])
ias_campo[2] = String(dw_1.Object.madu_codigo[il_Fila])
ias_campo[3] = dw_1.Object.madu_nombre[il_Fila]
ias_campo[4] = String(dw_1.Object.madu_candig[il_Fila])
ias_campo[5] = String(dw_1.Object.madu_valida[il_Fila])
ias_campo[6] = String(dw_1.Object.madu_operad[il_Fila])
ias_campo[7] = String(dw_1.Object.madu_inicio[il_Fila])
ias_campo[8] = String(dw_1.Object.madu_termin[il_Fila])

If (Not istr_mant.Agrega) And (Not istr_mant.Borra) Then
	dw_1.Object.madu_codigo.Protect	= 1
	dw_1.Object.madu_codigo.BackGround.Color =553648127
Else
	dw_1.Object.madu_codigo.Protect	= 0
	dw_1.Object.madu_codigo.BackGround.Color = RGB(255,255,255)
End If

dw_1.Object.espe_codigo[il_Fila] = Integer(istr_mant.Argumento[1])
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ctlcalmadurez
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ctlcalmadurez
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ctlcalmadurez
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ctlcalmadurez
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ctlcalmadurez
integer x = 1947
integer y = 368
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ctlcalmadurez
integer x = 1947
integer y = 152
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ctlcalmadurez
integer x = 1947
integer y = 584
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ctlcalmadurez
integer width = 1705
integer height = 952
string dataobject = "dw_mant_ctlcalmadurez"
end type

event dw_1::itemchanged;call super::itemchanged;Long	ll_Null
String	ls_Columna

ls_Columna	=	dwo.Name
SetNull(ll_Null)

Choose Case ls_Columna	
	Case "madu_codigo"
		If Duplicado(data) Then
			This.SetItem(Row, ls_Columna, ll_Null)
			Return 1
		End If

	Case 'madu_candig'
		If Integer(Data) > 15 Or Integer(Data) < 1 Then
			MessageBox('Alerta...', 'La cantidad a digitar debe estar entre 1 y 15.', StopSign!, Ok!)
			This.SetItem(Row, ls_Columna, ll_Null)
			Return 1
		End If
		
	Case 'madu_valida'
		This.Object.madu_operad[Row]	= ll_Null
		This.Object.madu_inicio[Row]		= ll_Null
		This.Object.madu_termin[Row]	= ll_Null
		
	Case 'madu_operad'
		If Data <> '4' Then
			This.SetItem(Row, 'madu_termin', ll_Null)
		End If

End Choose
end event

