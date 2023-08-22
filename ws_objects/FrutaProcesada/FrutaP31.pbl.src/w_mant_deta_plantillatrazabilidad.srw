$PBExportHeader$w_mant_deta_plantillatrazabilidad.srw
forward
global type w_mant_deta_plantillatrazabilidad from w_mant_detalle
end type
end forward

global type w_mant_deta_plantillatrazabilidad from w_mant_detalle
integer width = 2546
integer height = 1832
end type
global w_mant_deta_plantillatrazabilidad w_mant_deta_plantillatrazabilidad

type variables
DataWindowChild   idwc_variedad, idwc_ubicacion
end variables

forward prototypes
public function boolean duplicado (string campo, integer tipo)
end prototypes

public function boolean duplicado (string campo, integer tipo);Boolean	lb_Retorno
Long		ll_fila
String		ls_actividad, ls_ubicacion, ls_variedad

ls_actividad = String(dw_1.Object.acti_codigo[1])
ls_ubicacion = String(dw_1.Object.ubic_codigo[1])
ls_variedad = String(dw_1.Object.vari_codigo[1])

Choose Case Tipo
	Case 1
		ls_actividad	= campo
		
	Case 2
		ls_ubicacion = campo
		
	Case 3
		ls_variedad = campo
		
End Choose

ll_fila	= istr_mant.dw.Find("acti_codigo = " + ls_Actividad + " And ubic_codigo = " + ls_Ubicacion + " And vari_codigo = " + ls_Variedad, 1, istr_mant.dw.RowCount())

If ll_fila > 0 and ll_fila <> il_FilaAnc Then
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	lb_Retorno = True
Else
	lb_Retorno = False
End If

Return lb_Retorno
end function

on w_mant_deta_plantillatrazabilidad.create
call super::create
end on

on w_mant_deta_plantillatrazabilidad.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.clie_codigo[il_fila])
ias_campo[2] = String(dw_1.Object.plde_codigo[il_fila])
ias_campo[3] = String(dw_1.Object.esta_codigo[il_fila])
ias_campo[4] = String(dw_1.Object.espe_codigo[il_fila])
ias_campo[5] = String(dw_1.Object.acti_codigo[il_fila])
ias_campo[6] = String(dw_1.Object.ubic_codigo[il_fila])
ias_campo[7] = String(dw_1.Object.vari_codigo[il_fila])
ias_campo[8] = String(dw_1.Object.plac_tiemin[il_fila], 'dd/mm/yyyy hh:mm')
ias_campo[9] = String(dw_1.Object.plac_tiemax[il_fila], 'dd/mm/yyyy hh:mm')
ias_campo[10] = String(dw_1.Object.plac_fechas[il_fila], 'dd/mm/yyyy hh:mm')
ias_campo[11] = String(dw_1.Object.plac_activo[il_fila])
ias_campo[12] = dw_1.Object.usua_codigo[il_fila]

If Not istr_mant.agrega And Not istr_mant.borra Then
	dw_1.Object.acti_codigo.protect	= 1
	dw_1.Object.acti_codigo.Color 		= 0
	dw_1.Object.acti_codigo.BackGround.Color = 553648127
	dw_1.Object.ubic_codigo.protect	= 1
	dw_1.Object.ubic_codigo.Color 	= 0
	dw_1.Object.ubic_codigo.BackGround.Color = 553648127
	dw_1.Object.vari_codigo.protect	= 1
	dw_1.Object.vari_codigo.Color 		= 0
	dw_1.Object.vari_codigo.BackGround.Color = 553648127
Else
	dw_1.Object.clie_codigo[il_Fila] = Integer(istr_Mant.Argumento[1])
	dw_1.Object.plde_codigo[il_Fila] = Integer(istr_Mant.Argumento[2])
	dw_1.Object.esta_codigo[il_Fila] = Integer(istr_Mant.Argumento[3])
	dw_1.Object.espe_codigo[il_Fila] = Integer(istr_Mant.Argumento[4])

	dw_1.Object.plac_fechas[il_fila] = Today()
	dw_1.Object.usua_codigo[il_fila] = gstr_us.Nombre
End If

end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.SetItem(il_fila, "clie_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "esta_codigo", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "acti_codigo", Integer(ias_campo[5]))
	dw_1.SetItem(il_fila, "ubic_codigo", Integer(ias_campo[6]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[7]))
	dw_1.SetItem(il_fila, "plac_tiemin", Datetime(ias_campo[8]))
	dw_1.SetItem(il_fila, "plac_tiemax", Datetime(ias_campo[9]))
	dw_1.SetItem(il_fila, "plac_fechas", Datetime(ias_campo[10]))
	dw_1.SetItem(il_fila, "plac_activo", Integer(ias_campo[11]))
	dw_1.SetItem(il_fila, "usua_codigo", ias_campo[12])
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String		ls_mensaje, ls_colu[]

If Isnull(dw_1.Object.acti_codigo[il_fila]) Or dw_1.Object.acti_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Actividad"
	ls_colu[li_cont]	= "acti_codigo"
End If

If Isnull(dw_1.Object.ubic_codigo[il_fila]) Or dw_1.Object.ubic_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Ubicacion"
	ls_colu[li_cont]	= "ubic_codigo"
End If

If Isnull(dw_1.Object.vari_codigo[il_fila]) Or dw_1.Object.vari_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Variedad"
	ls_colu[li_cont]	= "vari_codigo"
End If


If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.clie_codigo[il_Fila] = Integer(istr_Mant.Argumento[1])
dw_1.Object.plde_codigo[il_Fila] = Integer(istr_Mant.Argumento[2])
dw_1.Object.esta_codigo[il_Fila] = Integer(istr_Mant.Argumento[3])
dw_1.Object.espe_codigo[il_Fila] = Integer(istr_Mant.Argumento[4])

dw_1.Object.plac_fechas[il_Fila] = Today()
dw_1.Object.usua_codigo[il_Fila] = gstr_us.Nombre
end event

event open;call super::open;dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.Retrieve(Integer(istr_Mant.Argumento[4]))

dw_1.GetChild("ubic_codigo", idwc_ubicacion)
idwc_ubicacion.SetTransObject(Sqlca)
idwc_ubicacion.Retrieve(Integer(istr_Mant.Argumento[2]))
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_plantillatrazabilidad
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_plantillatrazabilidad
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_plantillatrazabilidad
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_plantillatrazabilidad
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_plantillatrazabilidad
integer x = 2171
integer y = 552
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_plantillatrazabilidad
integer x = 2167
integer y = 356
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_plantillatrazabilidad
integer x = 2167
integer y = 776
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_plantillatrazabilidad
integer x = 91
integer width = 1865
integer height = 1528
string dataobject = "dw_mant_trazabilidadplantilla"
end type

event dw_1::itemchanged;call super::itemchanged;String ls_Columna, ls_null

SetNull(ls_null)

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "acti_codigo"
		If Duplicado(Data, 1) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case "ubic_codigo"
		If Duplicado(Data, 2) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If

	Case "vari_codigo"
		If Duplicado(Data, 3) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
End Choose
end event

