$PBExportHeader$w_mant_deta_termografo.srw
forward
global type w_mant_deta_termografo from w_mant_detalle
end type
end forward

global type w_mant_deta_termografo from w_mant_detalle
integer width = 2606
integer height = 1432
string title = "DESTINOS SAG"
end type
global w_mant_deta_termografo w_mant_deta_termografo

type variables
DataWindowChild	idwc_cliente, idwc_planta

Integer 	ii_tipoter
String	is_codigo



end variables

forward prototypes
public function boolean duplicado ()
end prototypes

public function boolean duplicado ();Long		ll_fila
String	ls_codigo

ll_Fila	=	istr_mant.dw.Find("term_codigo = '" + is_codigo + "'" + &
					  " AND term_tipter = " + String(ii_tipoter), 1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_termografo.create
call super::create
end on

on w_mant_deta_termografo.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;IF istr_mant.agrega = False AND istr_mant.borra = False THEN
	ias_campo[1] = String(dw_1.GetItemNumber(il_fila, "clie_codigo"))
	ias_campo[2] = String(dw_1.GetItemNumber(il_fila, "plde_codigo"))
	ias_campo[3] = dw_1.GetItemString(il_fila, "term_codigo")
	ias_campo[4] = String(dw_1.GetItemNumber(il_fila, "term_tipter"))
	ias_campo[5] = dw_1.GetItemString(il_fila, "term_descrip")
	ias_campo[6] = String(dw_1.GetItemNumber(il_fila, "term_condic"))
	ias_campo[7] = String(dw_1.GetItemNumber(il_fila, "term_estado"))
	dw_1.Object.term_condic.Protect 	=	0
ELSEIF istr_mant.agrega = True THEN
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "term_tipter", Integer(2))
	dw_1.SetItem(il_fila, "term_condic", Integer(1))
	dw_1.SetItem(il_fila, "term_estado", Integer(1))
	dw_1.Object.term_condic.Protect 	=	1
END IF

dw_1.SetFocus()
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "clie_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "term_codigo", (ias_campo[3]))
	dw_1.SetItem(il_fila, "term_tipter", Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "term_descrip", ias_campo[5])
	dw_1.SetItem(il_fila, "term_condic", Integer(ias_campo[6]))
	dw_1.SetItem(il_fila, "term_estado", integer(ias_campo[7]))

END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.GetItemString(il_fila, "Term_codigo")) OR dw_1.GetItemString(il_fila, "term_codigo") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Termógrafo"
	ls_colu[li_cont]	= "Term_codigo"
END IF

IF IsNull(dw_1.GetItemNumber(il_Fila, "term_tipter")) OR dw_1.GetItemNumber(il_Fila, "term_tipter") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTip'Termógrafo"
	ls_colu[li_cont]	= "term_tipter"	
END IF

IF Isnull(dw_1.GetItemString(il_fila, "term_descrip")) OR  dw_1.GetItemString(il_fila, "term_descrip") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nDescripción"
	ls_colu[li_cont]	= "term_descrip"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "term_condic")) OR  dw_1.GetItemNumber(il_fila, "term_condic") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCondición"
	ls_colu[li_cont]	= "term_condic"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "term_estado")) OR  dw_1.GetItemNumber(il_fila, "term_estado") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEstado"
	ls_colu[li_cont]	= "term_estado"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
ii_tipoter 	= 0
is_codigo	= ''
end event

event open;call super::open;dw_1.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()

//dw_1.InsertRow(0)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()




end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_termografo
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_termografo
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_termografo
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_termografo
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_termografo
integer x = 2194
integer y = 484
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_termografo
integer x = 2190
integer y = 268
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

//IF istr_mant.agrega THEN
//	Parent.TriggerEvent("ue_nuevo")
//	dw_1.Object.term_tipter[il_fila] = 2
//	dw_1.Object.term_condic[il_fila] = 1
//	dw_1.Object.term_estado[il_fila] = 1
//	
//ELSE
//	CloseWithReturn(Parent, istr_mant)
//END IF
//

If istr_mant.Agrega Then
	ComparteDatos(il_Fila, il_FilaAnc, dw_1, istr_mant.Dw)
	Parent.TriggerEvent("ue_nuevo")
	dw_1.Object.term_tipter[il_fila] = 2
	dw_1.Object.term_condic[il_fila] = 1
	dw_1.Object.term_estado[il_fila] = 1
Else
	If Not istr_mant.Borra Then
		ComparteDatos(il_Fila, il_FilaAnc, dw_1, istr_mant.Dw)
	End If
	
	CloseWithReturn(Parent, istr_mant)
End If
end event

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_termografo
integer x = 2190
integer y = 700
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_termografo
integer x = 91
integer width = 1929
integer height = 1108
string dataobject = "dw_mant_termografo"
end type

event dw_1::itemchanged;String	ls_columna, Lista
Long		ll_Nula

SetNull(ll_Nula)

ls_columna 	= 	dwo.name

CHOOSE CASE ls_columna
	CASE "term_codigo"
		ii_tipoter	=	dw_1.Object.term_tipter[row]
		is_codigo 	= 	data

		IF NOT duplicado() THEN
			is_codigo = data
		ELSE
			dw_1.SetItem(row, "term_codigo", String(ll_Nula))
			RETURN 1
		END IF	
		
	CASE "term_tipter"
		ii_tipoter	=	Integer(data)
		is_codigo 	= 	dw_1.Object.term_codigo[row]
		IF NOT duplicado() THEN
			ii_tipoter = Integer(data)
		ELSE
			dw_1.SetItem(row, "term_tipter", Integer(ll_Nula))
			RETURN 1
		END IF	
	
	CASE "term_condic"
		IF dw_1.Object.Term_estado[row] <> 1 THEN
			MessageBox("Error","Para Modificar Termógrafo debe Estar en Existencia",Information!, Ok!)
			dw_1.Object.term_Estado[row] = 1
			//dw_1.SetItem(row, "term_condic", Integer(ll_Nula))
			//RETURN 1
		ELSE
			IF data = '2' THEN
				dw_1.Object.term_Estado[row] = 3
			ELSEIF data = '1' THEN
				dw_1.Object.term_Estado[row] = 1
			END IF	
		END IF	
END CHOOSE
end event

