$PBExportHeader$w_mant_deta_defectosverifica.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_deta_defectosverifica from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_defectosverifica from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2377
integer height = 1156
end type
global w_mant_deta_defectosverifica w_mant_deta_defectosverifica

type variables
DataWindowChild	 	idwc_especie

uo_Familia		iuo_Familia
uo_SubFamilia	iuo_SFamilia
end variables

forward prototypes
public function boolean duplicados (string campo, string valor)
end prototypes

public function boolean duplicados (string campo, string valor);Long	ll_fila, ll_Familia, ll_Codigo

ll_Familia	=	dw_1.Object.ccfa_codigo[il_fila]
ll_Codigo		=	dw_1.Object.deva_codigo[il_fila]

Choose Case Campo
	Case "ccfa_codigo"
		ll_Familia	=	Long(valor)
		
	Case "deva_codigo"
		ll_Codigo	=	Long(valor)
	
End Choose

ll_fila	= dw_1.Find("ccfa_codigo = " + String(ll_Familia) + ' And deva_codigo = ' + String(ll_Codigo),1, dw_1.RowCount())
							
If ll_fila > 0 And ll_fila <> il_fila Then
	MessageBox("Atención","Registro ya fue ingresado",Information!, Ok!)
	Return True
Else
	Return False
End If
end function

on w_mant_deta_defectosverifica.create
call super::create
end on

on w_mant_deta_defectosverifica.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;

If Not istr_mant.agrega And Not istr_mant.borra Then	
	ias_campo[1]	= 	String(dw_1.Object.espe_codigo[il_Fila])
	ias_campo[2]	= 	String(dw_1.Object.ccfa_codigo[il_Fila])
	ias_campo[3]	= 	String(dw_1.Object.deva_codigo[il_Fila])
	ias_campo[4]	= 	String(dw_1.Object.ccsf_codigo[il_Fila])
	ias_campo[5]	= 	dw_1.Object.ccsf_descrip[il_Fila]
	ias_campo[6]	= 	dw_1.Object.deva_nombre[il_Fila]
	ias_campo[7]	= 	String(dw_1.Object.deva_ordena[il_Fila])
ElseIf istr_mant.agrega Then 
	dw_1.Object.espe_codigo[il_Fila] = Integer(istr_mant.argumento[2])
END IF
end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.Object.espe_codigo[il_Fila]	=	Integer(ias_campo[1])
	dw_1.Object.ccfa_codigo[il_Fila]	=	Integer(ias_campo[2])
	dw_1.Object.deva_codigo[il_Fila]	=	Integer(ias_campo[3])
	dw_1.Object.ccsf_codigo[il_Fila]	=	Integer(ias_campo[4])
	dw_1.Object.ccsf_descrip[il_Fila]	=	ias_campo[5]
	dw_1.Object.deva_nombre[il_Fila]	=	ias_campo[6]
	dw_1.Object.deva_ordena[il_Fila]	=	Integer(ias_campo[7])
End If
end event

event ue_antesguardar;String	ls_mensaje, ls_colu[]
Integer	li_cont

If IsNull(dw_1.Object.deva_codigo[il_fila]) OR dw_1.Object.deva_codigo[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCodigo Defecto"
	ls_colu[li_cont]	= "deva_codigo"
End If

If IsNull(dw_1.Object.ccfa_codigo[il_fila]) OR dw_1.Object.ccfa_codigo[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCodigo Familia"
	ls_colu[li_cont]	= "ccfa_codigo"
End If

If IsNull(dw_1.Object.deva_ordena[il_fila]) OR dw_1.Object.deva_ordena[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nOrdenamiento"
	ls_colu[li_cont]	= "deva_ordena"
End If

If IsNull(dw_1.Object.deva_nombre[il_fila]) OR dw_1.Object.deva_nombre[il_fila] = "" Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nDescripción Defecto"
	ls_colu[li_cont]	= "deva_nombre"
End If

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.espe_codigo[il_Fila] = Integer(istr_mant.argumento[2])


end event

event open;This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()

iuo_Familia		=	Create uo_Familia
iuo_SFamilia	=	Create uo_SubFamilia

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_defectosverifica
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_defectosverifica
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_defectosverifica
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_defectosverifica
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_defectosverifica
integer x = 2034
integer y = 400
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_defectosverifica
integer x = 2030
integer y = 184
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_defectosverifica
integer x = 2030
integer y = 616
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_defectosverifica
integer x = 96
integer y = 96
integer width = 1778
integer height = 904
string dataobject = "dw_mant_defectoverifica"
boolean maxbox = true
end type

event dw_1::itemchanged;String   ls_Null, ls_Columna

SetNull(ls_Null)

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "ccfa_codigo"
		If Not iuo_Familia.Existe(Integer(Data), True, Sqlca) Or Duplicados(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case "deva_codigo"
		If Duplicados(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case "ccsf_codigo"
		If Not iuo_SFamilia.Existe(Integer(Data), iuo_Familia.Codigo, True, Sqlca) Or Duplicados(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			This.Object.ccsf_descrip[Row] = iuo_SFamilia.Nombre
		End If
		
End Choose


end event

event dw_1::itemerror;call super::itemerror;RETURN 1
end event

event dw_1::buttonclicked;call super::buttonclicked;
end event

