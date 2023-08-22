$PBExportHeader$w_cons_inspeccion.srw
$PBExportComments$Genera Archivo Plano SAG de Eliminación de Pallets.
forward
global type w_cons_inspeccion from w_mant_tabla
end type
type st_3 from statictext within w_cons_inspeccion
end type
type em_fechad from editmask within w_cons_inspeccion
end type
type st_2 from statictext within w_cons_inspeccion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_cons_inspeccion
end type
type st_1 from statictext within w_cons_inspeccion
end type
type em_fechah from editmask within w_cons_inspeccion
end type
type st_4 from statictext within w_cons_inspeccion
end type
type em_desde from editmask within w_cons_inspeccion
end type
type st_5 from statictext within w_cons_inspeccion
end type
type em_hasta from editmask within w_cons_inspeccion
end type
type cbx_todos from checkbox within w_cons_inspeccion
end type
end forward

global type w_cons_inspeccion from w_mant_tabla
integer width = 2825
integer height = 1908
string title = "APROBACIÓN O RECHAZO LOTES SAG"
event ue_validapassword ( )
st_3 st_3
em_fechad em_fechad
st_2 st_2
uo_selcliente uo_selcliente
st_1 st_1
em_fechah em_fechah
st_4 st_4
em_desde em_desde
st_5 st_5
em_hasta em_hasta
cbx_todos cbx_todos
end type
global w_cons_inspeccion w_cons_inspeccion

type variables

end variables
event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

on w_cons_inspeccion.create
int iCurrent
call super::create
this.st_3=create st_3
this.em_fechad=create em_fechad
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.st_1=create st_1
this.em_fechah=create em_fechah
this.st_4=create st_4
this.em_desde=create em_desde
this.st_5=create st_5
this.em_hasta=create em_hasta
this.cbx_todos=create cbx_todos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.em_fechad
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.uo_selcliente
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.em_fechah
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.em_desde
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.em_hasta
this.Control[iCurrent+11]=this.cbx_todos
end on

on w_cons_inspeccion.destroy
call super::destroy
destroy(this.st_3)
destroy(this.em_fechad)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.st_1)
destroy(this.em_fechah)
destroy(this.st_4)
destroy(this.em_desde)
destroy(this.st_5)
destroy(this.em_hasta)
destroy(this.cbx_todos)
end on

event open;call super::open;boolean 				lb_Cerrar
uo_CargoTecnico	luo_Cargo

luo_Cargo	=	Create uo_CargoTecnico

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If Not luo_Cargo.of_ValidaCambio(gi_CodPlanta, gstr_us.Nombre, True, Sqlca) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	
	em_FechaD.Text = String(Today())
	em_FechaH.Text = String(Today())
	
	TriggerEvent('ue_validapassword')
	
End If

end event

event ue_nuevo();//istr_mant.borra			= False
//istr_mant.agrega			= True
//
//OpenWithParm(iw_mantencion, istr_mant)
//
//IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
//	pb_eliminar.Enabled	= TRUE
//	pb_grabar.Enabled		= TRUE
//END IF
//
//dw_1.SetRow(il_fila)
//dw_1.SelectRow(il_fila,True)
//
//
end event

event ue_recuperadatos;Long	ll_fila, respuesta,ll_fila1, ll_Desde, ll_Hasta

dw_1.Reset()

If cbx_Todos.Checked Then
	ll_Desde = 0
	ll_Hasta = 99999999
Else
	ll_Desde = Long(em_Desde.Text)
	ll_Hasta = Long(em_Hasta.Text)
End If

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, Date(em_FechaD.Text), Date(em_FechaH.Text), ll_Desde, ll_Hasta)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetFocus()
		pb_grabar.Enabled	= True
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

type dw_1 from w_mant_tabla`dw_1 within w_cons_inspeccion
integer x = 37
integer y = 672
integer width = 2249
integer height = 1016
integer taborder = 0
string dataobject = "dw_cons_inspeccion"
boolean hscrollbar = true
end type

event dw_1::getfocus;//
end event

event dw_1::losefocus;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::clicked;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_cons_inspeccion
integer x = 37
integer y = 32
integer width = 2249
integer height = 632
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_cons_inspeccion
integer x = 2437
integer y = 48
integer taborder = 0
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_cons_inspeccion
boolean visible = false
integer x = 2437
integer y = 348
integer taborder = 50
boolean enabled = false
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_cons_inspeccion
boolean visible = false
integer x = 2437
integer y = 520
integer height = 248
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_cons_inspeccion
boolean visible = false
integer x = 2437
integer y = 700
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_cons_inspeccion
integer x = 2437
integer y = 880
integer taborder = 70
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_cons_inspeccion
boolean visible = false
integer x = 2432
integer y = 1056
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_cons_inspeccion
integer x = 2437
integer y = 1444
integer taborder = 90
end type

type st_3 from statictext within w_cons_inspeccion
integer x = 133
integer y = 176
integer width = 274
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

type em_fechad from editmask within w_cons_inspeccion
integer x = 507
integer y = 328
integer width = 507
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_2 from statictext within w_cons_inspeccion
integer x = 133
integer y = 344
integer width = 261
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
string text = "F. Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_cons_inspeccion
event destroy ( )
integer x = 507
integer y = 160
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_1 from statictext within w_cons_inspeccion
integer x = 1070
integer y = 344
integer width = 261
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
string text = "F. Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fechah from editmask within w_cons_inspeccion
integer x = 1413
integer y = 328
integer width = 507
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_4 from statictext within w_cons_inspeccion
integer x = 133
integer y = 512
integer width = 334
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
string text = "Nro. Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_cons_inspeccion
integer x = 507
integer y = 496
integer width = 507
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
boolean dropdowncalendar = true
end type

type st_5 from statictext within w_cons_inspeccion
integer x = 1070
integer y = 512
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
string text = "Nro. Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_hasta from editmask within w_cons_inspeccion
integer x = 1413
integer y = 496
integer width = 507
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
boolean dropdowncalendar = true
end type

type cbx_todos from checkbox within w_cons_inspeccion
integer x = 1984
integer y = 508
integer width = 274
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
	em_Hasta.Enabled = False
	em_Desde.Text= ""
em_Hasta.Text = ""
Else
	em_Desde.Enabled = True
	em_Hasta.Enabled = True
	em_Desde.Text= "1"
	em_Hasta.Text = "99999999"
End If
end event

