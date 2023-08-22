$PBExportHeader$w_mant_deta_fechassemana.srw
forward
global type w_mant_deta_fechassemana from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_fechassemana from w_mant_detalle_csd
integer width = 1787
integer height = 928
string title = "FECHA PRIMERA SEMANA AÑO"
end type
global w_mant_deta_fechassemana w_mant_deta_fechassemana

type variables

end variables

on w_mant_deta_fechassemana.create
call super::create
end on

on w_mant_deta_fechassemana.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.fese_nroano[il_Fila])
ias_campo[2] = String(dw_1.Object.fese_fecini[il_Fila], 'dd/mm/yyyy')

IF Not istr_mant.agrega THEN
	dw_1.Object.fese_nroano.Protect	=	1
	dw_1.Object.fese_nroano.Color 	= RGB(255,255,255)
	dw_1.Object.fese_nroano.BackGround.Color = 553648127
END IF

end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "fese_nroano", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "fese_fecini", Date(ias_campo[2]))
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;String	ls_mensaje
Long		ll_columna

IF Isnull(dw_1.Object.fese_nroano[il_Fila]) Or dw_1.Object.fese_nroano[il_Fila] = 0 THEN
	ls_mensaje = "Código de Tipo Camión"
	ll_columna = 1
END IF

IF ll_columna > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ll_columna)
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_fechassemana
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_fechassemana
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_fechassemana
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_fechassemana
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_fechassemana
integer x = 1376
integer y = 276
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_fechassemana
integer x = 1376
integer y = 88
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_fechassemana
integer x = 1376
integer y = 460
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_fechassemana
integer x = 96
integer y = 128
integer width = 1216
integer height = 644
string dataobject = "dw_mant_fechassemana"
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_null, ll_fila
String		ls_campo

ls_campo = dwo.Name
SetNull(ll_null)

CHOOSE CASE ls_campo
	CASE "fese_nroano"
		ll_fila = This.Find("fese_nroano = " + Data, 1, This.RowCount())
		IF ll_fila > 0 and ll_fila <> il_fila THEN
			MessageBox("Error","Código de Año ya fue ingresado anteriormente",Information!, Ok!)
			This.SetItem(Row, ls_Campo, ll_null)
			return 1
		END IF
END CHOOSE
end event

