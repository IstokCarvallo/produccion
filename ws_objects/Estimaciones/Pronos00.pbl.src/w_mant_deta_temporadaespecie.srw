$PBExportHeader$w_mant_deta_temporadaespecie.srw
forward
global type w_mant_deta_temporadaespecie from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_temporadaespecie from w_mant_detalle_csd
integer width = 1984
integer height = 1112
end type
global w_mant_deta_temporadaespecie w_mant_deta_temporadaespecie

type variables
uo_especie	iuo_Especies
end variables

forward prototypes
public function boolean duplicado (string campo, integer tipo)
end prototypes

public function boolean duplicado (string campo, integer tipo);Long		ll_fila
Integer	li_Especie, li_Temporada

li_Especie		=	dw_1.Object.espe_codigo[il_Fila]
li_Temporada	=	dw_1.Object.pate_tempor[il_Fila]

Choose Case Tipo
	Case 1
		li_Especie = Integer(Campo)
		
	Case 2
		li_Temporada = Integer(Campo)
		
End Choose 

ll_fila	= dw_1.Find("espe_codigo = " + String(li_Especie ) + ' And pate_tempor = ' + String(li_Temporada),1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Agrónomo  ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_temporadaespecie.create
call super::create
end on

on w_mant_deta_temporadaespecie.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.pate_tempor[il_Fila])
ias_campo[2] = String(dw_1.Object.espe_codigo[il_Fila])
ias_campo[3] = String(dw_1.Object.pate_inicio[il_Fila], 'dd/mm/yyyy')
ias_campo[4] = String(dw_1.Object.pate_finali[il_Fila], 'dd/mm/yyyy')

If Not istr_mant.Agrega And Not istr_mant.Borra Then 
	dw_1.Object.pate_tempor.Protect	= 1
	dw_1.Object.espe_codigo.Protect	= 1
	dw_1.Object.pate_tempor.Color 	= RGB(255,255,255)
	dw_1.Object.espe_codigo.Color 	= RGB(255,255,255)
	dw_1.Object.pate_tempor.BackGround.Color	= 553648127
	dw_1.Object.espe_codigo.BackGround.Color	= 553648127
Else
	dw_1.Object.pate_tempor[il_Fila] = Integer(istr_Mant.Argumento[1])
End If
end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.Object.pate_tempor[il_Fila]	=	Integer(ias_campo[1])
	dw_1.Object.espe_codigo[il_Fila]	=	Integer(ias_campo[2])
	dw_1.Object.pate_inicio[il_Fila]	=	Datetime(ias_campo[3])
	dw_1.Object.pate_finali[il_Fila]		=	Datetime(ias_campo[4])
End If
end event

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_mens[], ls_colu[]

If IsNull(dw_1.Object.pate_tempor[il_Fila]) OR dw_1.Object.pate_tempor[il_Fila] = 0 Then
	li_cont ++
	ls_mensaje = ls_mensaje + "~nCódigo de Temporada"
	ls_colu[li_cont] = "pate_tempor"
End If

If IsNull(dw_1.Object.espe_codigo[il_Fila]) OR dw_1.Object.espe_codigo[il_Fila] = 0 Then
	li_cont ++
	ls_mensaje = ls_mensaje + "~nCódigo de Especie"
	ls_colu[li_cont] = "espe_codigo"
End If

If IsNull(dw_1.Object.pate_inicio[il_Fila]) Then
	li_cont ++
	ls_mensaje = ls_mensaje + "~nInicio de Temporada"
	ls_colu[li_cont] = "pate_inicio"
End If

If IsNull(dw_1.Object.pate_finali[il_Fila]) Then
	li_cont ++
	ls_mensaje = ls_mensaje + "~nTermino de Temporada"
	ls_colu[li_cont] = "pate_finali"
End If

If li_cont > 0 Then	
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

event open;call super::open;iuo_Especies	=	Create uo_especie
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.pate_tempor[il_Fila] = Integer(istr_Mant.Argumento[1])
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_temporadaespecie
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_temporadaespecie
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_temporadaespecie
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_temporadaespecie
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_temporadaespecie
integer x = 1673
integer y = 344
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_temporadaespecie
integer x = 1673
integer y = 140
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_temporadaespecie
integer x = 1678
integer y = 544
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_temporadaespecie
integer y = 100
integer width = 1435
integer height = 760
string dataobject = "dw_mant_temporadaespecie"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna

ls_columna = dwo.name

Choose Case ls_columna
	Case "espe_codigo"
		If Duplicado(Data, 1) Or Not iuo_Especies.Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(il_fila, ls_columna, Long(ias_campo[1]))
			Return 1
		Else
			This.Object.espe_nombre[Row] = iuo_Especies.Nombre
		End If
End Choose
end event

event dw_1::buttonclicked;call super::buttonclicked;String	ls_Boton
Str_Busqueda lstr_Busq

ls_Boton = dwo.name

Choose Case ls_Boton
	Case "b_especie"
		
		OpenWithParm(w_busc_especies, lstr_Busq)
		
		lstr_Busq = Message.PowerObjectParm
		
		If UpperBound(lstr_Busq.Argum) > 0 Then
			This.Object.espe_codigo[Row]		= Integer(lstr_Busq.Argum[1])
			This.Object.espe_nombre[Row]	= lstr_Busq.Argum[2]
		End if

End Choose
end event

