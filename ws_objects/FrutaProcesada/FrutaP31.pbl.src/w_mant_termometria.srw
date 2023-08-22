$PBExportHeader$w_mant_termometria.srw
forward
global type w_mant_termometria from window
end type
type cb_todos_e from commandbutton within w_mant_termometria
end type
type cb_elimina from commandbutton within w_mant_termometria
end type
type cb_agrega from commandbutton within w_mant_termometria
end type
type cb_todos_a from commandbutton within w_mant_termometria
end type
type dw_3 from uo_dw within w_mant_termometria
end type
type cbx_proceso from checkbox within w_mant_termometria
end type
type cb_proceso from commandbutton within w_mant_termometria
end type
type st_3 from statictext within w_mant_termometria
end type
type em_proceso from editmask within w_mant_termometria
end type
type st_2 from statictext within w_mant_termometria
end type
type uo_selcamara from uo_seleccion_camarasbode within w_mant_termometria
end type
type cb_buscar from commandbutton within w_mant_termometria
end type
type st_8 from statictext within w_mant_termometria
end type
type em_numero from editmask within w_mant_termometria
end type
type cbx_numero from checkbox within w_mant_termometria
end type
type st_5 from statictext within w_mant_termometria
end type
type em_desde from editmask within w_mant_termometria
end type
type st_1 from statictext within w_mant_termometria
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_termometria
end type
type pb_grabar from picturebutton within w_mant_termometria
end type
type pb_lectura from picturebutton within w_mant_termometria
end type
type dw_1 from uo_dw within w_mant_termometria
end type
type pb_salir from picturebutton within w_mant_termometria
end type
type pb_nuevo from picturebutton within w_mant_termometria
end type
type st_encabe from statictext within w_mant_termometria
end type
end forward

global type w_mant_termometria from window
integer x = 5
integer y = 16
integer width = 4293
integer height = 2076
boolean titlebar = true
string title = "Cambio de Temperaturas a Pallet en Pre-frio"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowstate windowstate = maximized!
long backcolor = 16777215
event ue_asignacion ( )
cb_todos_e cb_todos_e
cb_elimina cb_elimina
cb_agrega cb_agrega
cb_todos_a cb_todos_a
dw_3 dw_3
cbx_proceso cbx_proceso
cb_proceso cb_proceso
st_3 st_3
em_proceso em_proceso
st_2 st_2
uo_selcamara uo_selcamara
cb_buscar cb_buscar
st_8 st_8
em_numero em_numero
cbx_numero cbx_numero
st_5 st_5
em_desde em_desde
st_1 st_1
uo_selplanta uo_selplanta
pb_grabar pb_grabar
pb_lectura pb_lectura
dw_1 dw_1
pb_salir pb_salir
pb_nuevo pb_nuevo
st_encabe st_encabe
end type
global w_mant_termometria w_mant_termometria

type variables
Long 		il_fila
Date		id_FechaAcceso
Time		it_HoraAcceso

uo_Termometria	iuo_Termometria

Menu		im_menu

Str_parms		istr_parms
Str_mant			istr_mant
Str_busqueda	istr_busq
Str_info			istr_info
end variables

forward prototypes
public subroutine wf_todos (datawindow source, ref datawindow target)
public subroutine wf_unico (datawindow source, ref datawindow target)
end prototypes

event ue_asignacion();Integer				columna, li_fila
Str_busqueda		lstr_busq

il_fila	=	dw_1.GetRow()

dw_1.AcceptText()

lstr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
lstr_busq.argum[2]	=	String(dw_1.Object.plde_codigo[il_fila])
lstr_busq.argum[3]	=	String(dw_1.Object.paen_numero[il_fila])


OpenWithParm(w_cons_movtosxpallet, lstr_busq)
end event

public subroutine wf_todos (datawindow source, ref datawindow target);Long ll_Nueva, ll_Planta, ll_Camara, ll_Proceso, ll_Pallet

If Source.RowCount() < 1 Then Return

Do
	Source.SelectRow(Source.GetRow(), False)
	
	ll_Planta		= Source.GetItemNumber(Source.GetRow(), "plde_codigo")
	ll_Camara	= Source.GetItemNumber(Source.GetRow(), "cama_codigo")
	ll_Proceso	= Source.GetItemNumber(Source.GetRow(), "teen_numero")
	ll_Pallet		= Source.GetItemNumber(Source.GetRow(), "paen_numero")
			
	If Target.Find("paen_numero = " + String(ll_Pallet), 1, Target.Rowcount()) = 0 Then
		
		ll_Nueva = Target.InsertRow(0)
	
		Target.SetItem(ll_Nueva, "plde_codigo", ll_Planta)
		Target.SetItem(ll_Nueva, "cama_codigo", ll_Camara)
		Target.SetItem(ll_Nueva, "teen_numero", ll_proceso)
		Target.SetItem(ll_Nueva, "paen_numero", ll_Pallet)
	End If
	
	Source.DeleteRow(Source.GetRow())
	
Loop Until Source.RowCount() < 1
end subroutine

public subroutine wf_unico (datawindow source, ref datawindow target);Long ll_New, ll_Planta, ll_Camara, ll_Proceso, ll_Pallet

If Source.RowCount() < 1 Then Return
Source.SelectRow(Source.GetRow(), False)

ll_Planta		= Source.GetItemNumber(Source.GetRow(), "plde_codigo")
ll_Camara	= Source.GetItemNumber(Source.GetRow(), "cama_codigo")
ll_Proceso	= Source.GetItemNumber(Source.GetRow(), "teen_numero")
ll_Pallet		= Source.GetItemNumber(Source.GetRow(), "paen_numero")
		
If Target.Find("paen_numero =" + String(ll_Pallet), 1, Target.Rowcount()) = 0 Then
	
	ll_New = Target.InsertRow(0)

	Target.SetItem(ll_New, "plde_codigo", ll_Planta)
	Target.SetItem(ll_New, "cama_codigo", ll_Camara)
	Target.SetItem(ll_New, "teen_numero", ll_proceso)
	Target.SetItem(ll_New, "paen_numero", ll_Pallet)
End If

Source.DeleteRow(Source.GetRow())
end subroutine

on w_mant_termometria.create
this.cb_todos_e=create cb_todos_e
this.cb_elimina=create cb_elimina
this.cb_agrega=create cb_agrega
this.cb_todos_a=create cb_todos_a
this.dw_3=create dw_3
this.cbx_proceso=create cbx_proceso
this.cb_proceso=create cb_proceso
this.st_3=create st_3
this.em_proceso=create em_proceso
this.st_2=create st_2
this.uo_selcamara=create uo_selcamara
this.cb_buscar=create cb_buscar
this.st_8=create st_8
this.em_numero=create em_numero
this.cbx_numero=create cbx_numero
this.st_5=create st_5
this.em_desde=create em_desde
this.st_1=create st_1
this.uo_selplanta=create uo_selplanta
this.pb_grabar=create pb_grabar
this.pb_lectura=create pb_lectura
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_nuevo=create pb_nuevo
this.st_encabe=create st_encabe
this.Control[]={this.cb_todos_e,&
this.cb_elimina,&
this.cb_agrega,&
this.cb_todos_a,&
this.dw_3,&
this.cbx_proceso,&
this.cb_proceso,&
this.st_3,&
this.em_proceso,&
this.st_2,&
this.uo_selcamara,&
this.cb_buscar,&
this.st_8,&
this.em_numero,&
this.cbx_numero,&
this.st_5,&
this.em_desde,&
this.st_1,&
this.uo_selplanta,&
this.pb_grabar,&
this.pb_lectura,&
this.dw_1,&
this.pb_salir,&
this.pb_nuevo,&
this.st_encabe}
end on

on w_mant_termometria.destroy
destroy(this.cb_todos_e)
destroy(this.cb_elimina)
destroy(this.cb_agrega)
destroy(this.cb_todos_a)
destroy(this.dw_3)
destroy(this.cbx_proceso)
destroy(this.cb_proceso)
destroy(this.st_3)
destroy(this.em_proceso)
destroy(this.st_2)
destroy(this.uo_selcamara)
destroy(this.cb_buscar)
destroy(this.st_8)
destroy(this.em_numero)
destroy(this.cbx_numero)
destroy(this.st_5)
destroy(this.em_desde)
destroy(this.st_1)
destroy(this.uo_selplanta)
destroy(this.pb_grabar)
destroy(this.pb_lectura)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_nuevo)
destroy(this.st_encabe)
end on

event open;Boolean	lb_Cerrar
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 180")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCamara.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	iuo_Termometria	=	Create uo_Termometria
	
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCamara.Seleccion(True, False)
	
	uo_SelPlanta.Codigo = gi_codplanta
	uo_SelPlanta.dw_Seleccion.Object.Codigo[1] = gi_codplanta
	uo_SelCamara.Filtra(gi_codplanta)	
	
	em_Desde.Text				=	String(Today())
End If
end event

event close;Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= This.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF

IF li_vta = 1 THEN
	This.ParentWindow().ToolBarVisible	= False
	im_menu.Item[1].Item[6].Enabled		= False
	im_menu.Item[7].Visible					= False
END IF

GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

event resize;Integer		li_posic_x, li_posic_y, li_visible, &
				li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

dw_1.Resize(((This.WorkSpaceWidth() - 510) / 2) - 100,This.WorkSpaceHeight() - dw_1.y - 75)

dw_1.x					= 78
dw_3.x					= dw_1.x + dw_1.Width + 200
dw_3.y 					= dw_1.y
dw_3.Height 			= dw_1.Height
dw_3.Width 			= dw_1.Width

st_encabe.x				= 78
st_encabe.width		= This.WorkSpaceWidth() - 510

cb_todos_a.x			=	dw_1.x + dw_1.Width + 15
cb_todos_e.x			=	cb_todos_a.x
cb_agrega.x				=	cb_todos_a.x
cb_elimina.x				=	cb_todos_a.x

IF st_encabe.Visible THEN
	li_posic_y				=	st_encabe.y
ELSE
	li_posic_y				=	dw_1.y
END IF

li_posic_x				=	This.WorkSpaceWidth() - 370

pb_lectura.x				=	li_posic_x
pb_lectura.y				=	li_posic_y
pb_lectura.width		=	li_Ancho
pb_lectura.height		=	li_Alto
li_posic_y 				+= li_Siguiente * 1.25

IF pb_nuevo.Visible THEN
	pb_nuevo.x			=	li_posic_x
	pb_nuevo.y			=	li_posic_y
	pb_nuevo.width	=	li_Ancho
	pb_nuevo.height	=	li_Alto
	li_visible++
	li_posic_y 			+= li_Siguiente
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x			=	li_posic_x
	pb_grabar.y			=	li_posic_y
	pb_grabar.width	=	li_Ancho
	pb_grabar.height	=	li_Alto
	li_visible++
	li_posic_y 			+= li_Siguiente
END IF

pb_salir.x				=	li_posic_x
pb_salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_salir.width			=	li_Ancho
pb_salir.height			=	li_Alto
end event

type cb_todos_e from commandbutton within w_mant_termometria
integer x = 1751
integer y = 1312
integer width = 174
integer height = 80
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<<"
end type

event clicked;wf_Todos(dw_3, dw_1)
end event

type cb_elimina from commandbutton within w_mant_termometria
integer x = 1751
integer y = 1244
integer width = 174
integer height = 80
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<"
end type

event clicked;wf_Unico(dw_3, dw_1)
end event

type cb_agrega from commandbutton within w_mant_termometria
integer x = 1751
integer y = 1028
integer width = 174
integer height = 80
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = ">"
end type

event clicked;wf_Unico(dw_1, dw_3)
end event

type cb_todos_a from commandbutton within w_mant_termometria
integer x = 1751
integer y = 956
integer width = 174
integer height = 80
integer taborder = 90
string dragicon = "AppIcon!"
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = ">>"
end type

event clicked;wf_Todos(dw_1, dw_3)
end event

type dw_3 from uo_dw within w_mant_termometria
event ue_mousemove pbm_mousemove
integer x = 1975
integer y = 556
integer width = 1687
integer height = 1404
integer taborder = 140
string dragicon = "\Desarrollo 12\Imagenes\Varios PNG\Row.ico"
boolean titlebar = true
string title = "Cambio de Nro. Pallet"
string dataobject = "dw_carga_palletfrio"
boolean hscrollbar = true
end type

event ue_mousemove;//If This.IsSelected(This.GetRow()) And Message.WordParm = 1 Then This.Drag(begin!)
end event

event clicked;call super::clicked;IF Row > 0 THEN
	This.SelectRow(0, False)
	This.SetRow(Row)
	This.ScrollToRow(Row) 
	This.SelectRow(Row, True)
END IF
end event

event dragdrop;call super::dragdrop;DataWindow	ldw_Source
Long				ll_nueva, ll_Planta, ll_Camara, ll_Proceso, ll_Pallet

If Source.typeof() = DataWindow! Then
	ldw_Source = Source
	If Source = dw_3 Then
		This.RowsMove(ldw_Source.GetRow(), ldw_Source.GetRow(), Primary!, This, Row, Primary!)
		Return
	End If
Else
	Return
End If

ldw_Source.SelectRow(ldw_Source.GetRow(), False)

ll_Planta		= ldw_Source.GetItemNumber(ldw_Source.GetRow(), "plde_codigo")
ll_Camara	= ldw_Source.GetItemNumber(ldw_Source.GetRow(), "cama_codigo")
ll_Proceso	= ldw_Source.GetItemNumber(ldw_Source.GetRow(), "teen_numero")
ll_Pallet		= ldw_Source.GetItemNumber(ldw_Source.GetRow(), "paen_numero")
		
If This.Find("paen_numero =" + String(ll_Pallet), 1, This.Rowcount()) = 0 Then
	
	ll_nueva = This.InsertRow(row)

	This.SetItem(ll_nueva, "plde_codigo", ll_Planta)
	This.SetItem(ll_nueva, "cama_codigo", ll_Camara)
	This.SetItem(ll_nueva, "teen_numero", ll_proceso)
	This.SetItem(ll_nueva, "paen_numero", ll_Pallet)
End If

ldw_Source.DeleteRow(ldw_Source.GetRow())
end event

type cbx_proceso from checkbox within w_mant_termometria
integer x = 2519
integer y = 232
integer width = 261
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
	cb_Proceso.Enabled	= False
	em_Proceso.Enabled	= False
	em_Proceso.Text		= ''
Else
	cb_Proceso.Enabled	= True
	em_Proceso.Enabled	= True
	em_Proceso.Text		= ''
End If
end event

type cb_proceso from commandbutton within w_mant_termometria
integer x = 2359
integer y = 220
integer width = 119
integer height = 100
integer taborder = 70
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

event clicked;str_busqueda	lstr_Busq

lstr_busq.argum[1]	= String(uo_SelPlanta.Codigo)

OpenWithParm(w_busc_procesofrio, lstr_busq)

lstr_busq = Message.PowerObjectParm

if UpperBound(lstr_busq.Argum) < 3 Then Return

If lstr_busq.Argum[2] <> "" Then	
	em_desde.Text		=	lstr_busq.argum[1]
	em_proceso.Text 	=	lstr_busq.argum[2]
	uo_SelCamara.Todos(False)
	uo_SelCamara.Codigo = Integer(lstr_busq.argum[3])
	uo_SelCamara.dw_Seleccion.Object.Codigo[1] = Integer(lstr_busq.argum[3])
End If

end event

type st_3 from statictext within w_mant_termometria
integer x = 1417
integer y = 236
integer width = 384
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
string text = "Nro. Proceso"
boolean focusrectangle = false
end type

type em_proceso from editmask within w_mant_termometria
integer x = 1847
integer y = 216
integer width = 494
integer height = 112
integer taborder = 40
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

event modified;If iuo_Termometria.Existe(uo_SelPlanta.Codigo, Long(This.Text), True, Sqlca) Then
	em_desde.Text		=	String(iuo_Termometria.Fecha, 'dd/mm/yyyy')
	em_proceso.Text 	=	String(iuo_Termometria.Proceso)

	uo_SelCamara.Todos(False)
	uo_SelCamara.Codigo = iuo_Termometria.Camara
	uo_SelCamara.dw_Seleccion.Object.Codigo[1] = iuo_Termometria.Camara
Else
	MessageBox("Alerta", "Codigo de Proceso no Existe", Information!, ok!)
	Return
End If
end event

type st_2 from statictext within w_mant_termometria
integer x = 146
integer y = 312
integer width = 242
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
string text = "Camara"
boolean focusrectangle = false
end type

type uo_selcamara from uo_seleccion_camarasbode within w_mant_termometria
event destroy ( )
integer x = 407
integer y = 228
integer taborder = 20
end type

on uo_selcamara.destroy
call uo_seleccion_camarasbode::destroy
end on

type cb_buscar from commandbutton within w_mant_termometria
integer x = 2359
integer y = 364
integer width = 119
integer height = 100
integer taborder = 60
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

event clicked;str_busqueda	lstr_Busq

lstr_busq.argum[1]	=	String(gi_CodExport)
lstr_busq.argum[5]   = ''

OpenWithParm(w_busc_palletencab, lstr_busq)

lstr_busq = Message.PowerObjectParm

if UpperBound(lstr_busq.Argum) < 2 Then Return
If lstr_busq.Argum[2] <> "" Then	em_numero.Text 			=	lstr_busq.argum[2]

end event

type st_8 from statictext within w_mant_termometria
integer x = 1417
integer y = 376
integer width = 210
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
string text = "Pallet"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_termometria
integer x = 1847
integer y = 356
integer width = 494
integer height = 112
integer taborder = 50
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

type cbx_numero from checkbox within w_mant_termometria
integer x = 2519
integer y = 368
integer width = 261
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
	cb_buscar.Enabled = False
	em_numero.Enabled = False
	em_numero.Text = ''
Else
	cb_buscar.Enabled = True
	em_numero.Enabled = True
	em_numero.Text = ''
End If
end event

type st_5 from statictext within w_mant_termometria
integer x = 1417
integer y = 108
integer width = 206
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
string text = "Desde"
boolean focusrectangle = false
end type

type em_desde from editmask within w_mant_termometria
integer x = 1847
integer y = 96
integer width = 443
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_1 from statictext within w_mant_termometria
integer x = 146
integer y = 104
integer width = 238
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
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_mant_termometria
event destroy ( )
integer x = 407
integer y = 96
integer height = 88
integer taborder = 10
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return 

Choose Case This.Codigo
	Case -1, -9
		uo_SelCamara.Filtra(0)
		
	Case Else
		uo_SelCamara.Filtra(This.Codigo)
		
End Choose
end event

type pb_grabar from picturebutton within w_mant_termometria
string tag = "Selección de Parámetros"
integer x = 3918
integer y = 856
integer width = 302
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Long ll_Fila, ll_Planta, ll_Camara, ll_Proceso, ll_Origen, ll_Destino

If dw_3.RowCount() = 0 Then Return

SetPointer(HourGlass!)

For ll_Fila = 1 To dw_3.RowCount()
	ll_Planta		= dw_3.Object.plde_codigo[ll_Fila]
	ll_Camara	= dw_3.Object.cama_codigo[ll_Fila]
	ll_Proceso	= dw_3.Object.teen_numero[ll_Fila]
	ll_Origen		= dw_3.Object.paen_numero[ll_Fila]
	ll_Destino	= dw_3.Object.paen_nrodes[ll_Fila]
	
	If dw_3.Object.Marca[ll_Fila] = 0 And Not IsNull(ll_Destino) Then
		If Not iuo_Termometria.Existe(ll_Planta, ll_Camara, ll_Proceso, ll_Destino, True, Sqlca) Then
			DECLARE CambioPallet PROCEDURE FOR dbo.Traz_CambioPalletPreFrio
					@Planta = :ll_Planta, 
					@Camara = :ll_Camara, 
					@Proceso = :ll_Proceso, 
					@Origen = :ll_Origen, 
					@Destino = :ll_Destino;
			EXECUTE CambioPallet;
					
			If sqlca.SQLCode = -1 Then
				F_ErrorBaseDatos(sqlca, "Lectura Procedimiento Traz_CambioPalletPreFrio")
				Rollback;
			Else
				dw_3.Object.Marca[ll_Fila] = 1
				Commit;			
			End If	
		End If	
	End If
Next

SetPointer(Arrow!)
end event

type pb_lectura from picturebutton within w_mant_termometria
string tag = "Selección de Parámetros"
integer x = 3909
integer y = 596
integer width = 302
integer height = 244
integer taborder = 90
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Busqueda.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Busqueda-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Long	ll_Fila, ll_Proceso = -1, ll_Numero = -1

If Not cbx_numero.Checked Then ll_Numero = Long(em_Numero.Text)
If Not cbx_proceso.Checked Then ll_Proceso = Long(em_Proceso.Text)

pb_nuevo.TriggerEvent(clicked!)

ll_Fila = dw_1.Retrieve(uo_SelPlanta.Codigo, uo_SelCamara.Codigo, ll_Proceso, ll_Numero, Date(em_desde.Text))
	
IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox('ADVERTENCIA','No Existe información para este período', Information!, OK!)
	Return
End If
end event

type dw_1 from uo_dw within w_mant_termometria
event ue_mousemove pbm_mousemove
integer x = 37
integer y = 556
integer width = 1687
integer height = 1404
integer taborder = 130
string dragicon = "\Desarrollo 12\Imagenes\Varios PNG\Row.ico"
boolean titlebar = true
string title = "Detalle de Pallet"
string dataobject = "dw_mues_palletfrio"
boolean hscrollbar = true
end type

event ue_mousemove;If This.IsSelected(GetRow()) And Message.WordParm = 1 Then 
	This.Drag(begin!)
End If

end event

event dragdrop;call super::dragdrop;DataWindow	ldw_Source

If source.TypeOf() = DataWindow! Then
	ldw_Source 	= 	source
	If Source 	=	dw_1 Then
		Return
	End If
Else
	Return
End If

ldw_Source.DeleteRow(ldw_Source.GetRow())
end event

event clicked;call super::clicked;IF row > 0 THEN
	This.SelectRow(0, False)
	This.SelectRow(row, True)
END IF
end event

type pb_salir from picturebutton within w_mant_termometria
integer x = 3909
integer y = 1600
integer width = 302
integer height = 244
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event pb_salir::clicked;call super::clicked;Close(parent)
end event

type pb_nuevo from picturebutton within w_mant_termometria
string tag = "Selección de Parámetros"
integer x = 3913
integer y = 1352
integer width = 302
integer height = 244
integer taborder = 110
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Documento.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Documento-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;dw_1.Reset()
dw_3.Reset()

end event

type st_encabe from statictext within w_mant_termometria
integer x = 37
integer y = 48
integer width = 2757
integer height = 468
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

