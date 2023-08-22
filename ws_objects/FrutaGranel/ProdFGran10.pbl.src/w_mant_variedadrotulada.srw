$PBExportHeader$w_mant_variedadrotulada.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_variedadrotulada from w_mant_directo
end type
type st_2 from statictext within w_mant_variedadrotulada
end type
type st_1 from statictext within w_mant_variedadrotulada
end type
type em_numero from editmask within w_mant_variedadrotulada
end type
type st_3 from statictext within w_mant_variedadrotulada
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_variedadrotulada
end type
type uo_plantas from uo_seleccion_plantas within w_mant_variedadrotulada
end type
end forward

global type w_mant_variedadrotulada from w_mant_directo
integer x = 155
integer y = 156
integer width = 2601
integer height = 1880
string title = "MANTENEDOR DE ROTULACION FRUTA GRANEL"
event ue_validaborrar ( )
st_2 st_2
st_1 st_1
em_numero em_numero
st_3 st_3
uo_selcliente uo_selcliente
uo_plantas uo_plantas
end type
global w_mant_variedadrotulada w_mant_variedadrotulada

type variables
Integer	ii_tipo, ii_Contador

uo_Variedades	iuo_Variedad



end variables

forward prototypes
public function boolean existepallet (long al_numero)
end prototypes

public function boolean existepallet (long al_numero);Long		ll_pallet

IF al_numero <> 0 THEN

		SELECT paen_numero
		INTO	:ll_pallet
		FROM	dbo.spro_palletencab
		WHERE	plde_codigo =	:uo_Plantas.Codigo
		AND	clie_codigo		=	:uo_SelCliente.Codigo
		AND	paen_numero	=	:al_numero;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_Palletencab")
		em_numero.Text = ''
		em_numero.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Número de Pallet Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.Text = ''
		em_numero.SetFocus()
		RETURN False
	END IF
	RETURN TRUE
END IF	
	
end function

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_Plantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	ii_tipo	=	Integer(Message.StringParm)
	
	uo_SelCliente.Seleccion(False, False)
	uo_Plantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_Plantas.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	iuo_Variedad	=	Create uo_Variedades
End If
end event

on w_mant_variedadrotulada.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.em_numero=create em_numero
this.st_3=create st_3
this.uo_selcliente=create uo_selcliente
this.uo_plantas=create uo_plantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_numero
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.uo_selcliente
this.Control[iCurrent+6]=this.uo_plantas
end on

on w_mant_variedadrotulada.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.uo_selcliente)
destroy(this.uo_plantas)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(long(em_Numero.Text), uo_SelCliente.Codigo, uo_Plantas.Codigo)
	
	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
	pb_insertar.Enabled = True
END IF














end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

		
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF

	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Rotulada"
		ls_colu[li_cont]	= "pafr_varrot"
	END IF
	
	IF IsNull(dw_1.Object.prod_codrot[il_fila]) OR dw_1.Object.prod_codrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor Rotulado"
		ls_colu[li_cont]	= "prod_codrot"
	END IF
	
//	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ''	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Rotulado"
//		ls_colu[li_cont]	= "pafr_calrot"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Rotulado"
//		ls_colu[li_cont]	= "pafr_huert4"
//	END IF	
//
//	IF IsNull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Rotulado"
//		ls_colu[li_cont]	= "pafr_cuart4"
//	END IF	
//
//	IF IsNull(dw_1.Object.pafr_rotpak[il_fila]) OR dw_1.Object.pafr_rotpak[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Rotulado"
//		ls_colu[li_cont]	= "pafr_rotpak"
//	END IF	
//	
//	IF dw_1.Object.pafr_fecrot[il_fila] = Date('19000101')	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de fecha embalaje Rotulada"
//		ls_colu[li_cont]	= "pafr_fecrot"
//	END IF		
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
//		ii_contador = 1
//		
//		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
//			ii_contador++
//		loop 
//		ii_contador --
//		dw_1.SetRow(ii_contador)
//		Message.DoubleParm = -1
	END IF
END IF





end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

		
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Rotulada"
		ls_colu[li_cont]	= "pafr_varrot"
	END IF
	
	IF IsNull(dw_1.Object.prod_codrot[il_fila]) OR dw_1.Object.prod_codrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor Rotulado"
		ls_colu[li_cont]	= "prod_codrot"
	END IF
	
//	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ''	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Rotulado"
//		ls_colu[li_cont]	= "pafr_calrot"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Rotulado"
//		ls_colu[li_cont]	= "pafr_huert4"
//	END IF	
//
//	IF IsNull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Rotulado"
//		ls_colu[li_cont]	= "pafr_cuart4"
//	END IF	
//
//	IF IsNull(dw_1.Object.pafr_rotpak[il_fila]) OR dw_1.Object.pafr_rotpak[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Rotulado"
//		ls_colu[li_cont]	= "pafr_rotpak"
//	END IF	
//	
//	IF dw_1.Object.pafr_fecrot[il_fila] = Date('19000101')	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Fecha Embalaje Rotulada"
//		ls_colu[li_cont]	= "pafr_fecrot"
//	END IF		
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	END IF
END IF





end event

type st_encabe from w_mant_directo`st_encabe within w_mant_variedadrotulada
integer x = 87
integer y = 60
integer width = 1943
integer height = 416
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_variedadrotulada
integer x = 2185
integer y = 428
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;em_numero.Enabled	=	TRUE

uo_SelCliente.Bloquear(False)
uo_Plantas.Bloquear(False)

em_numero.BackColor=	RGB(255, 255, 255)
em_numero.text = ""
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_variedadrotulada
integer x = 2181
integer taborder = 50
end type

event pb_lectura::clicked;uo_SelCliente.Bloquear(True)
uo_Plantas.Bloquear(True)

IF IsNull(integer(em_numero.text)) OR integer(em_numero.text) = 0 THEN
	MessageBox("Atención","Debe Seleccionar Nº de Guía Previamente",Exclamation!)
	em_numero.SetFocus()
	RETURN
ELSE
	em_numero.Enabled	=	FALSE
	em_numero.BackColor=	RGB(192, 192, 192)
	Parent.PostEvent("ue_recuperadatos")
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_variedadrotulada
boolean visible = false
integer x = 2181
integer y = 788
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_variedadrotulada
boolean visible = false
integer x = 2181
integer y = 608
integer taborder = 80
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_variedadrotulada
integer x = 2181
integer y = 1532
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_variedadrotulada
boolean visible = false
integer x = 2181
integer y = 1148
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_variedadrotulada
integer x = 2181
integer y = 968
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_variedadrotulada
integer x = 87
integer y = 496
integer width = 1943
integer height = 1228
integer taborder = 60
string dataobject = "dw_mues_variedadrotulada"
boolean hscrollbar = true
end type

event dw_1::clicked;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

	
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0	THEN
	END IF
	
	IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0	THEN
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
		ii_contador = 1
		
		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
			ii_contador++
		loop 
		ii_contador --
		dw_1.SetRow(ii_contador)
		Message.DoubleParm = -1
	END IF
END IF

IF dw_1.rowcount() > 0 THEN
	IF dw_1.Object.Paen_numero[il_fila] = 0 THEN
		dw_1.SetRow(ii_contador)
	END IF
END IF
end event

event dw_1::itemchanged;call super::itemchanged;String 	ls_Columna, ls_Null

SetNull(ls_Null)
dw_1.AcceptText()

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "pafr_varrot"
		IF Not iuo_Variedad.Existe(This.Object.espe_codigo[Row], Integer(Data), True, Sqlca)	THEN
			dw_1.SetItem(Row, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF		
END CHOOSE




end event

event dw_1::rowfocuschanged;integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type st_2 from statictext within w_mant_variedadrotulada
integer x = 270
integer y = 228
integer width = 293
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_variedadrotulada
integer x = 270
integer y = 112
integer width = 293
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_variedadrotulada
integer x = 608
integer y = 328
integer width = 402
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF ExistePallet(Long(em_numero.text)) = False THEN
	This.SetFocus()
END IF
end event

type st_3 from statictext within w_mant_variedadrotulada
integer x = 270
integer y = 344
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_variedadrotulada
event destroy ( )
integer x = 608
integer y = 100
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_plantas from uo_seleccion_plantas within w_mant_variedadrotulada
event destroy ( )
integer x = 608
integer y = 216
integer height = 92
integer taborder = 60
boolean bringtotop = true
end type

on uo_plantas.destroy
call uo_seleccion_plantas::destroy
end on

