$PBExportHeader$w_mant_deta_alternaria.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_deta_alternaria from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_alternaria from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2107
integer height = 1096
end type
global w_mant_deta_alternaria w_mant_deta_alternaria

type variables
DataWindowChild	 	idwc_especie, idwc_variedad

uo_calibre   iuo_calibre


end variables

forward prototypes
public function boolean duplicados (string campo, string valor)
end prototypes

public function boolean duplicados (string campo, string valor);Long		ll_fila, ll_Especie

ll_Especie		=	dw_1.Object.ctal_codigo[il_fila]

CHOOSE CASE campo
	CASE "ctal_codigo"
		ll_Especie		=	Long(valor)
			
END CHOOSE

ll_fila	= dw_1.Find("ctal_codigo = " + String(ll_Especie),1, dw_1.RowCount())
							
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Atención","Registro ya fue ingresado",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_alternaria.create
call super::create
end on

on w_mant_deta_alternaria.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	= 	String(dw_1.Object.espe_codigo[il_Fila])
ias_campo[2]	= 	String(dw_1.Object.ctal_codigo[il_Fila])
ias_campo[3]	= 	dw_1.Object.ctal_nombre[il_Fila]
dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[1]))

If Not istr_mant.agrega Then
	dw_1.Object.ctal_codigo.Protect = 1
	dw_1.Object.ctal_codigo.Background.Color = Rgb(192,192,192)
End If
end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.Object.espe_codigo[il_Fila]	=	Integer(ias_campo[1])
	dw_1.Object.ctal_codigo[il_Fila]	=	Integer(ias_campo[2])
	dw_1.Object.ctal_nombre[il_Fila]	=	ias_campo[3]
End If
end event

event ue_antesguardar;String	ls_mensaje, ls_colu[]
Integer	li_cont

If IsNull(dw_1.Object.ctal_nombre[il_fila]) OR trim(dw_1.Object.ctal_nombre[il_fila]) = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNombre"
	ls_colu[li_cont]	= "ctal_nombre"
END IF

IF Isnull(dw_1.Object.ctal_codigo[il_fila]) OR dw_1.Object.ctal_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCodigo"
	ls_colu[li_cont]	= "ctal_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[1]))



end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_alternaria
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_alternaria
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_alternaria
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_alternaria
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_alternaria
integer x = 1806
integer y = 408
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_alternaria
integer x = 1801
integer y = 192
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_alternaria
integer x = 1801
integer y = 624
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_alternaria
integer x = 96
integer y = 96
integer width = 1550
integer height = 784
string dataobject = "dw_mant_alternaria"
boolean maxbox = true
end type

event dw_1::itemchanged;
String   ls_Null, ls_Columna

SetNull(ls_Null)
ls_Columna = dwo.Name

Choose Case ls_Columna	
	Case "ctal_codigo"
		If Duplicados(ls_Columna, Data) Then 
			This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
			Return 1
		End If

End Choose


end event

event dw_1::itemerror;call super::itemerror;RETURN 1
end event

