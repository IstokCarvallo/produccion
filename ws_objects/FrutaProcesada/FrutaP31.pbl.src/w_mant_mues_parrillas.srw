$PBExportHeader$w_mant_mues_parrillas.srw
$PBExportComments$Mantenedor de Técnicos
forward
global type w_mant_mues_parrillas from w_mant_tabla
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_parrillas
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_parrillas
end type
type st_3 from statictext within w_mant_mues_parrillas
end type
type st_7 from statictext within w_mant_mues_parrillas
end type
type st_8 from statictext within w_mant_mues_parrillas
end type
type em_guia from editmask within w_mant_mues_parrillas
end type
type cbx_guia from checkbox within w_mant_mues_parrillas
end type
type st_2 from statictext within w_mant_mues_parrillas
end type
type em_desde from editmask within w_mant_mues_parrillas
end type
type cbx_fecha from checkbox within w_mant_mues_parrillas
end type
type st_1 from statictext within w_mant_mues_parrillas
end type
type em_numero from editmask within w_mant_mues_parrillas
end type
type cbx_numero from checkbox within w_mant_mues_parrillas
end type
type cb_numero from commandbutton within w_mant_mues_parrillas
end type
end forward

global type w_mant_mues_parrillas from w_mant_tabla
integer width = 2798
integer height = 1920
string title = "Mantencion Parrillas / Pallets / Temperaturas"
event ue_validaregistro ( )
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
st_3 st_3
st_7 st_7
st_8 st_8
em_guia em_guia
cbx_guia cbx_guia
st_2 st_2
em_desde em_desde
cbx_fecha cbx_fecha
st_1 st_1
em_numero em_numero
cbx_numero cbx_numero
cb_numero cb_numero
end type
global w_mant_mues_parrillas w_mant_mues_parrillas

type variables




end variables

on w_mant_mues_parrillas.create
int iCurrent
call super::create
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.st_3=create st_3
this.st_7=create st_7
this.st_8=create st_8
this.em_guia=create em_guia
this.cbx_guia=create cbx_guia
this.st_2=create st_2
this.em_desde=create em_desde
this.cbx_fecha=create cbx_fecha
this.st_1=create st_1
this.em_numero=create em_numero
this.cbx_numero=create cbx_numero
this.cb_numero=create cb_numero
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selplanta
this.Control[iCurrent+2]=this.uo_selcliente
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_7
this.Control[iCurrent+5]=this.st_8
this.Control[iCurrent+6]=this.em_guia
this.Control[iCurrent+7]=this.cbx_guia
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.em_desde
this.Control[iCurrent+10]=this.cbx_fecha
this.Control[iCurrent+11]=this.st_1
this.Control[iCurrent+12]=this.em_numero
this.Control[iCurrent+13]=this.cbx_numero
this.Control[iCurrent+14]=this.cb_numero
end on

on w_mant_mues_parrillas.destroy
call super::destroy
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.em_guia)
destroy(this.cbx_guia)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.cbx_fecha)
destroy(this.st_1)
destroy(this.em_numero)
destroy(this.cbx_numero)
destroy(this.cb_numero)
end on

event ue_recuperadatos;Long	ll_fila, respuesta, ll_Folio = -1, ll_Guia = -1
Date	ld_Fecha


If Not cbx_numero.Checked Then ll_Folio = Long(em_Numero.Text)
If Not cbx_guia.Checked Then ll_Guia = Long(em_Guia.Text)
If Not cbx_Fecha.Checked Then ld_Fecha	=	Date(em_Desde.Text)

DO	
	ll_fila	=	dw_1.Retrieve(uo_SelPlanta.Codigo, uo_SelCliente.Codigo, ld_Fecha, ll_Folio, ll_Guia)
		
	IF ll_fila 	=	-1 THEN
		respuesta =	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		pb_grabar.Enabled	= True
		pb_imprimir.Enabled	= True
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
END IF
end event

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	
	uo_SelPlanta.Todos(False)
	
	uo_SelCliente.Codigo = gi_codexport
	uo_SelCliente.dw_Seleccion.Object.Codigo[1] = gi_codexport
	
	uo_SelPlanta.Codigo = gi_codplanta
	uo_SelPlanta.dw_Seleccion.Object.Codigo[1] = gi_codplanta
	
	em_desde.Text				=	String(Today())
End If
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_parrillas
integer x = 73
integer y = 604
integer width = 2190
integer height = 1116
integer taborder = 40
string dataobject = "dw_mant_mues_parrillas"
boolean hscrollbar = true
end type

event dw_1::rowfocuschanged;Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

RETURN 0
end event

event dw_1::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)

RETURN 0
end event

event dw_1::getfocus;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_parrillas
integer x = 73
integer width = 2190
integer height = 516
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_parrillas
integer x = 2450
integer taborder = 20
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_parrillas
integer x = 2446
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled	= False
pb_imprimir.Enabled	= False

il_fila					= 0


end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_parrillas
boolean visible = false
integer x = 2446
integer taborder = 30
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_parrillas
boolean visible = false
integer x = 2446
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_parrillas
integer x = 2446
integer taborder = 50
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_parrillas
boolean visible = false
integer x = 2446
integer taborder = 60
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_parrillas
integer x = 2437
integer taborder = 90
end type

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_parrillas
event destroy ( )
integer x = 375
integer y = 240
integer taborder = 60
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_parrillas
event destroy ( )
integer x = 375
integer y = 120
integer height = 88
integer taborder = 60
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_3 from statictext within w_mant_mues_parrillas
integer x = 114
integer y = 132
integer width = 229
integer height = 64
integer taborder = 20
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

type st_7 from statictext within w_mant_mues_parrillas
integer x = 119
integer y = 328
integer width = 229
integer height = 64
integer taborder = 40
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

type st_8 from statictext within w_mant_mues_parrillas
integer x = 1330
integer y = 448
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Guia"
boolean focusrectangle = false
end type

type em_guia from editmask within w_mant_mues_parrillas
integer x = 1682
integer y = 428
integer width = 494
integer height = 112
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type cbx_guia from checkbox within w_mant_mues_parrillas
integer x = 1682
integer y = 336
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_guia.Enabled = False
	em_guia.Text = ''
Else
	em_guia.Enabled = True
	em_guia.Text = ''
End If
end event

type st_2 from statictext within w_mant_mues_parrillas
integer x = 119
integer y = 452
integer width = 393
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "F. Recepción"
boolean focusrectangle = false
end type

type em_desde from editmask within w_mant_mues_parrillas
integer x = 539
integer y = 436
integer width = 425
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type cbx_fecha from checkbox within w_mant_mues_parrillas
integer x = 987
integer y = 444
integer width = 256
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Desde.Enabled = False
	em_Desde.Text = ''
Else
	em_Desde.Enabled = True
	em_Desde.Text	= String(Today(), 'dd/mm/yyyy')
End If
end event

type st_1 from statictext within w_mant_mues_parrillas
integer x = 1321
integer y = 200
integer width = 320
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Recepcion"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_mues_parrillas
integer x = 1682
integer y = 180
integer width = 494
integer height = 112
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type cbx_numero from checkbox within w_mant_mues_parrillas
integer x = 1682
integer y = 80
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_numero.Enabled	= False
	cb_numero.Enabled	= False
	em_numero.Text		= ''
Else
	em_numero.Enabled	= True
	cb_numero.Enabled	= True
	em_numero.Text		= ''
End If
end event

type cb_numero from commandbutton within w_mant_mues_parrillas
integer x = 2075
integer y = 80
integer width = 101
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;istr_busq.argum[1]	= String(uo_SelCliente.Codigo)
istr_busq.argum[2]	= String(uo_SelPlanta.Codigo)

OpenWithParm(w_busc_recfruprocee, istr_busq)

istr_busq	       = Message.PowerObjectParm

If istr_busq.Argum[8] <> "" Then
	em_numero.Text	= istr_busq.argum[5]
	em_desde.Text		= istr_busq.argum[6]
	em_guia.Text		= istr_busq.argum[8]
	cbx_fecha.Checked	=	False
	cbx_guia.Checked		=	False
	em_desde.Enabled	=	True
	em_guia.Enabled		=	True
End If
end event

