$PBExportHeader$w_mant_deta_eventoalarma.srw
forward
global type w_mant_deta_eventoalarma from w_mant_detalle
end type
end forward

global type w_mant_deta_eventoalarma from w_mant_detalle
integer width = 2382
integer height = 1040
end type
global w_mant_deta_eventoalarma w_mant_deta_eventoalarma

type variables

end variables

forward prototypes
public function boolean duplicado (string as_horario, integer ai_tipo)
end prototypes

public function boolean duplicado (string as_horario, integer ai_tipo);Long		ll_fila
String		ls_Hora

ls_Hora	= String(dw_1.GetItemTime(il_fila,"eval_horalr"), 'HH:MM')

Choose Case ai_Tipo
	Case	3
		ls_Hora = Mid(as_Horario, 1, 5)
		
End Choose 

ll_fila	= istr_mant.dw.Find("String(eval_horalr, 'HH:MM') =  '" + ls_Hora + "'", 1, istr_mant.dw.RowCount())

If ll_fila > 0 And ll_fila <> il_FilaAnc Then 
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	Return True
Else
	Return False
End If
end function

on w_mant_deta_eventoalarma.create
call super::create
end on

on w_mant_deta_eventoalarma.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemTime(il_fila, "eval_horalr"), 'hh:mm')
ias_campo[2] = dw_1.GetItemString(il_fila, "eval_nombre")
ias_campo[3] = String(dw_1.GetItemTime(il_fila, "eval_tolera"), 'hh:mm')


If Not  istr_mant.Agrega And Not istr_mant.Borra Then 
	dw_1.Object.eval_horalr.Protect 				= 1
	dw_1.Object.eval_horalr.BackGround.Color = 553648127
	dw_1.Object.eval_horalr.Color 					= RGB(255,255,255)
ElseIf istr_mant.Agrega Then
	dw_1.Object.eval_horalr[il_Fila]	= Now()
End If

end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.SetItem(il_fila, "eval_horalr", Time(ias_campo[1]))
	dw_1.SetItem(il_fila, "eval_nombre", ias_campo[2])
	dw_1.SetItem(il_fila, "eval_tolera", Time(ias_campo[3]))
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String		ls_mensaje, ls_colu[]

If IsNull(dw_1.GetItemTime(il_fila, "eval_horalr")) Then
	li_cont ++
	ls_mensaje		= ls_mensaje + "~nHorario de Alarma."
	ls_colu[li_cont]	= "eval_horalr"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.eval_horalr[il_Fila]	= Now()
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_eventoalarma
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_eventoalarma
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_eventoalarma
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_eventoalarma
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_eventoalarma
integer x = 2071
integer y = 420
integer taborder = 20
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_eventoalarma
integer x = 2071
integer y = 236
integer taborder = 10
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_eventoalarma
integer x = 2080
integer y = 596
integer taborder = 30
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_eventoalarma
integer width = 1787
integer height = 688
integer taborder = 0
string dataobject = "dw_mant_eventoalarma"
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_Null
String		ls_campo

ls_campo =dwo.Name

SetNull(ll_Null)

Choose Case ls_campo		
	Case "eval_horalr"
		If Duplicado(Data, 3) Then
			This.SetItem(il_fila, ls_campo, Time(ll_Null))
			Return 1
		End If
		
End Choose 
end event

