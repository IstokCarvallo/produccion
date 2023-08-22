$PBExportHeader$w_mant_deta_categorias_cert.srw
forward
global type w_mant_deta_categorias_cert from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_categorias_cert from w_mant_detalle_csd
integer width = 2071
integer height = 852
end type
global w_mant_deta_categorias_cert w_mant_deta_categorias_cert

type variables

end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long	ll_fila
String	ls_Certificadora

ls_Certificadora	=	String(dw_1.Object.cace_codigo[il_fila])

Choose Case as_Columna
	Case "cace_codigo"
		ls_Certificadora = as_valor
		
End Choose

ll_fila	= dw_1.Find("cace_codigo = " + ls_Certificadora, 1, dw_1.RowCount())
								
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_categorias_cert.create
call super::create
end on

on w_mant_deta_categorias_cert.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.cace_codigo[il_fila])
ias_campo[2] = dw_1.Object.cace_nombre[il_fila]
ias_campo[3] = dw_1.Object.cace_abrevi[il_fila]

If Not istr_mant.Agrega Then
	dw_1.Object.cace_codigo.Protect = 1
	dw_1.Object.cace_codigo.Color = 0
	dw_1.Object.cace_codigo.BackGround.Color = 553648127
End If
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	ias_campo[1] = String(dw_1.Object.cace_codigo[il_Fila])
	ias_campo[2] = dw_1.Object.cace_nombre[il_Fila]
	ias_campo[3] = dw_1.Object.cace_abrevi[il_Fila]
END IF
end event

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

If IsNull(dw_1.Object.cace_codigo[il_fila]) OR dw_1.Object.cace_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Categoria"
	ls_colu[li_cont]	= "cace_codigo"
End If

If IsNull(dw_1.Object.cace_nombre[il_fila]) OR dw_1.Object.cace_nombre[il_fila] = '' Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nNombre de Categoria"
	ls_colu[li_cont]	= "cace_nombre"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_categorias_cert
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_categorias_cert
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_categorias_cert
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_categorias_cert
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_categorias_cert
integer x = 1760
integer y = 300
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_categorias_cert
integer x = 1755
integer y = 84
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_categorias_cert
integer x = 1755
integer y = 516
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_categorias_cert
integer x = 110
integer y = 108
integer width = 1536
integer height = 628
string dataobject = "dw_mant_categorias_cert"
end type

event dw_1::itemchanged;call super::itemchanged;String		ls_columna
Integer	li_Null

ls_Columna	=	dwo.Name

SetNull(li_Null)

Choose Case ls_Columna
	Case "cace_codigo"
		If Duplicado(ls_Columna, data) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If
End Choose
end event

