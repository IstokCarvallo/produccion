$PBExportHeader$w_mant_deta_pronconectividad.srw
forward
global type w_mant_deta_pronconectividad from w_mant_detalle
end type
end forward

global type w_mant_deta_pronconectividad from w_mant_detalle
integer width = 2656
integer height = 1824
string title = "CONECTIVIDAD"
end type
global w_mant_deta_pronconectividad w_mant_deta_pronconectividad

type variables

end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String		ls_Conecta

ls_Conecta	=	String(dw_1.Object.prco_codigo[dw_1.GetRow()])

CHOOSE CASE as_Columna
	CASE "prco_codigo"
		ls_Conecta		=	as_Valor
	
END CHOOSE

ll_fila	= istr_mant.dw.Find("prco_codigo = " + ls_Conecta, 1 , istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN 
	MessageBox("Atención","Registro ya fue ingresado anteriormente.",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_pronconectividad.create
call super::create
end on

on w_mant_deta_pronconectividad.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemNumber(il_fila, "prco_codigo"))
ias_campo[2] = dw_1.GetItemString(il_fila, "prco_nombre")
ias_campo[3] = dw_1.GetItemString(il_fila, "prco_odbc")
ias_campo[4] = dw_1.GetItemString(il_fila, "prco_servid")
ias_campo[5] = dw_1.GetItemString(il_fila, "prco_baseda")
ias_campo[6] = dw_1.GetItemString(il_fila, "prco_usuari")
ias_campo[7] = dw_1.GetItemString(il_fila, "prco_passwo")
ias_campo[8] = dw_1.GetItemString(il_fila, "prco_dbms")
ias_campo[9] = String(dw_1.GetItemNumber(il_fila, "pate_tempor"))

IF Not istr_mant.Agrega THEN
	dw_1.Object.prco_codigo.Protect				=	1
	dw_1.Object.prco_codigo.Color 				= 	RGB(255,255,255)
	dw_1.Object.prco_codigo.BackGround.Color	= 563648127
END IF

end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "prco_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "prco_nombre", ias_campo[2])
	dw_1.SetItem(il_fila, "prco_odbc", ias_campo[3])
	dw_1.SetItem(il_fila, "prco_servid", ias_campo[4])
	dw_1.SetItem(il_fila, "prco_baseda", ias_campo[5])
	dw_1.SetItem(il_fila, "prco_usuari", ias_campo[6])
	dw_1.SetItem(il_fila, "prco_passwo", ias_campo[7])
	dw_1.SetItem(il_fila, "prco_dbms", ias_campo[8])
	dw_1.SetItem(il_fila, "pate_tempor", Integer(ias_campo[9]))
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;String	ls_mensaje, ls_colu[]
Long		li_cont

IF Isnull(dw_1.Object.prco_codigo[il_fila]) OR dw_1.Object.prco_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Conectividad"
	ls_colu[li_cont]	= "prco_codigo"
END IF

IF Isnull(dw_1.Object.prco_nombre[il_fila]) OR dw_1.Object.prco_nombre[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nDescripción de Conexion"
	ls_colu[li_cont]	= "prco_nombre"
END IF

IF Isnull(dw_1.Object.prco_dbms[il_fila]) OR dw_1.Object.prco_dbms[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nDescripción de DBMS"
	ls_colu[li_cont]	= "prco_dbms"
END IF

IF Isnull(dw_1.Object.prco_usuari[il_fila]) OR dw_1.Object.prco_usuari[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNombre de Usuario"
	ls_colu[li_cont]	= "prco_usuari"
END IF

IF Isnull(dw_1.Object.prco_passwo[il_fila]) OR dw_1.Object.prco_passwo[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nPassword de Usuario"
	ls_colu[li_cont]	= "prco_passwo"
END IF

If dw_1.Object.prco_dbms[il_fila] = 'ODBC' Then 
	IF Isnull(dw_1.Object.prco_odbc[il_fila]) OR dw_1.Object.prco_odbc[il_fila] = "" THEN
		li_cont	++
		ls_mensaje 			= ls_mensaje + "~nDescripción de ODBC"
		ls_colu[li_cont]	= "prco_odbc"
	END IF
	
	IF Isnull(dw_1.Object.prco_servid[il_fila]) OR dw_1.Object.prco_servid[il_fila] = "" THEN
		li_cont	++
		ls_mensaje 			= ls_mensaje + "~nNombre Servidor de Datos"
		ls_colu[li_cont]	= "prco_servid"
	END IF
	
	IF Isnull(dw_1.Object.prco_baseda[il_fila]) OR dw_1.Object.prco_baseda[il_fila] = "" THEN
		li_cont	++
		ls_mensaje 			= ls_mensaje + "~nNombre Base de Datos"
		ls_colu[li_cont]	= "prco_baseda"
	END IF
End If

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_pronconectividad
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_pronconectividad
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_pronconectividad
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_pronconectividad
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_pronconectividad
integer x = 2263
integer y = 300
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_pronconectividad
integer x = 2258
integer y = 84
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_pronconectividad
integer x = 2258
integer y = 516
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_pronconectividad
integer y = 76
integer width = 1966
integer height = 1556
string dataobject = "dw_mant_pronconectividad"
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_fila
String		ls_campo, ls_Null

SetNull(ls_Null)
ls_campo = dwo.Name

CHOOSE CASE ls_campo
	CASE "prco_codigo"
		If Duplicado(ls_Campo, Data) Then
			This.SetItem(Row, ls_Campo, long(ls_Null))
			Return 1
		End If
		
	Case 'prco_dbms'
		If Not Data = 'ODBC' Then
			This.Object.prco_odbc[Row]	= ls_Null
			This.Object.prco_servid[Row]	= ls_Null
		End If
END CHOOSE
end event

