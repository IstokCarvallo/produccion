$PBExportHeader$w_mant_deta_estaciontrabajo.srw
forward
global type w_mant_deta_estaciontrabajo from w_mant_detalle
end type
end forward

global type w_mant_deta_estaciontrabajo from w_mant_detalle
integer width = 2505
integer height = 1268
end type
global w_mant_deta_estaciontrabajo w_mant_deta_estaciontrabajo

type variables
DataWindowChild   idwc_destino
end variables

forward prototypes
public function boolean duplicado (string campo, integer tipo)
end prototypes

public function boolean duplicado (string campo, integer tipo);Boolean	lb_Retorno
Long		ll_fila
String		ls_codigo

Choose Case Tipo
	Case 1
		ls_codigo	= campo
		
End Choose

ll_fila	= istr_mant.dw.Find("esta_codigo = " + ls_codigo, 1, istr_mant.dw.RowCount())

If ll_fila > 0 and ll_fila <> il_FilaAnc Then
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	lb_Retorno = True
Else
	lb_Retorno = False
End If

Return lb_Retorno
end function

on w_mant_deta_estaciontrabajo.create
call super::create
end on

on w_mant_deta_estaciontrabajo.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.esta_codigo[il_fila])
ias_campo[2] = dw_1.Object.esta_nombre[il_fila]
ias_campo[3] = String(dw_1.Object.esta_tipest[il_fila])
ias_campo[4] = String(dw_1.Object.esta_fechac[il_fila], 'dd/mm/yyyy hh:mm')
ias_campo[5] = String(dw_1.Object.esta_activo[il_fila])
ias_campo[6] = dw_1.Object.usua_codigo[il_fila]

If Not istr_mant.agrega And Not istr_mant.borra Then
	dw_1.Object.esta_codigo.protect	= 1
	dw_1.Object.esta_codigo.Color		= Rgb(255,255,255)
	dw_1.Object.esta_codigo.BackGround.Color = 553648127
Else
	dw_1.Object.esta_fechac[il_fila] = Today()
	dw_1.Object.usua_codigo[il_fila] = gstr_us.Nombre
End If

end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.SetItem(il_fila, "esta_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "esta_nombre", ias_campo[2])
	dw_1.SetItem(il_fila, "esta_tipest", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "esta_fechac", Datetime(ias_campo[4]))
	dw_1.SetItem(il_fila, "esta_activo", Integer(ias_campo[5]))
	dw_1.SetItem(il_fila, "usua_codigo", ias_campo[6])
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String		ls_mensaje, ls_colu[]

If Isnull(dw_1.Object.esta_codigo[il_fila]) Or dw_1.Object.esta_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Estacion"
	ls_colu[li_cont]	= "esta_codigo"
End If

If Isnull(dw_1.Object.esta_nombre[il_fila]) Or dw_1.Object.esta_nombre[il_fila] = "" Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNombre Estacion Trabajo"
	ls_colu[li_cont]	= "esta_nombre"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.esta_fechac[il_Fila] = Today()
dw_1.Object.usua_codigo[il_Fila] = gstr_us.Nombre
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_estaciontrabajo
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_estaciontrabajo
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_estaciontrabajo
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_estaciontrabajo
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_estaciontrabajo
integer x = 2171
integer y = 552
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_estaciontrabajo
integer x = 2167
integer y = 356
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_estaciontrabajo
integer x = 2167
integer y = 776
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_estaciontrabajo
integer x = 91
integer width = 1851
integer height = 944
string dataobject = "dw_mant_estaciontrabajo"
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_null

SetNull(li_null)

Choose Case dwo.Name
	Case "esta_codigo"
		If Duplicado(data, 1) Then
			This.Object.esta_codigo[row] = li_Null
			Return 1
		End If
		
End Choose
end event

