$PBExportHeader$w_busc_embarque.srw
forward
global type w_busc_embarque from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
type cbx_entrefechas from checkbox within tabpage_3
end type
type st_y from statictext within tabpage_3
end type
type cbx_filtra from checkbox within tabpage_3
end type
type sle_argumento3 from editmask within tabpage_3
end type
type sle_argumento4 from editmask within tabpage_3
end type
end forward

global type w_busc_embarque from w_busqueda
integer x = 123
integer y = 304
integer width = 3566
integer height = 1808
string title = "Búsqueda de Pallets"
end type
global w_busc_embarque w_busc_embarque

type variables
String		is_TipoPallet, is_EstadoPallet, is_CondicPallet, is_Planta
DataWindowChild	idwc_planta

end variables

on w_busc_embarque.create
int iCurrent
call super::create
end on

on w_busc_embarque.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[2]	= dw_1.GetItemString(dw_1.GetRow(),"embq_codigo")
CloseWithReturn(This,istr_busq)
end event

event open;call super::open;//
Long		ll_fila = 1
datawindowchild dwc_planta
istr_busq 				=	Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()

tab_1.tabpage_1.dw_cliente.SetTransObject(sqlca)
tab_1.tabpage_1.dw_cliente.InsertRow(0)
tab_1.tabpage_1.dw_cliente.object.clie_codigo[1] = integer(istr_busq.Argum[1])

is_ordena = "Cod Embarque:embq_codigo,Cod recibidor:reci_nombre,Nombre Puerto:puer_nombre,Nombre Nave:embq_nomnav,Fecha Zarpe:embq_fzarpe"
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_embarque
boolean visible = false
integer x = 3205
integer y = 1124
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_embarque
integer x = 37
integer y = 680
integer width = 3022
integer taborder = 60
string dataobject = "dw_mues_embarques"
end type

event dw_1::doubleclicked;call super::doubleclicked;IF row > 0 THEN
	Parent.PostEvent("ue_asignacion")
END IF
end event

type pb_salir from w_busqueda`pb_salir within w_busc_embarque
integer x = 3200
integer y = 828
alignment htextalign = center!
end type

event pb_salir::clicked;istr_busq.argum[5] = ""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_embarque
integer x = 37
integer y = 32
integer width = 2555
integer height = 640
end type

event tab_1::selectionchanged;call super::selectionchanged;dw_1.SetFilter("")
dw_1.Filter()
end event

on tab_1.create
call super::create
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3}
end on

on tab_1.destroy
call super::destroy
end on

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2519
integer height = 512
string text = "Filtros              "
st_1 st_1
dw_cliente dw_cliente
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2075
integer y = 228
integer taborder = 20
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(integer(istr_busq.Argum[1])) < 1 THEN
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2519
integer height = 512
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2071
integer y = 304
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2519
integer height = 512
string text = "Búsqueda          "
cbx_entrefechas cbx_entrefechas
st_y st_y
cbx_filtra cbx_filtra
sle_argumento3 sle_argumento3
sle_argumento4 sle_argumento4
end type

on tabpage_3.create
this.cbx_entrefechas=create cbx_entrefechas
this.st_y=create st_y
this.cbx_filtra=create cbx_filtra
this.sle_argumento3=create sle_argumento3
this.sle_argumento4=create sle_argumento4
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cbx_entrefechas
this.Control[iCurrent+2]=this.st_y
this.Control[iCurrent+3]=this.cbx_filtra
this.Control[iCurrent+4]=this.sle_argumento3
this.Control[iCurrent+5]=this.sle_argumento4
end on

on tabpage_3.destroy
call super::destroy
destroy(this.cbx_entrefechas)
destroy(this.st_y)
destroy(this.cbx_filtra)
destroy(this.sle_argumento3)
destroy(this.sle_argumento4)
end on

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 594
integer y = 176
integer width = 402
long backcolor = 1073741824
end type

event sle_argumento2::getfocus;call super::getfocus;//
sle_argumento3.enabled = FALSE
st_y.enabled = FALSE
cbx_entrefechas.TriggerEvent("Clicked")
cbx_entrefechas.enabled = FALSE
This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento4.Text			= ""
sle_argumento4.BackColor	= RGB(166,180,210)
sle_argumento4.TabOrder		= 0
//
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer y = 176
integer width = 535
integer height = 88
boolean enabled = true
string text = "Codigo Embarque"
alignment alignment = right!
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
boolean visible = false
integer x = 1865
integer y = 0
integer width = 402
long backcolor = 1073741824
end type

event sle_argumento1::modified;is_argume	= This.Text
end event

event sle_argumento1::getfocus;call super::getfocus;//
sle_argumento3.enabled = true
st_y.enabled = true
cbx_entrefechas.TriggerEvent("Clicked")
cbx_entrefechas.enabled = true
//
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 0
integer y = 296
integer width = 585
string text = "Fecha de Proceso"
alignment alignment = right!
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2071
integer y = 304
end type

event pb_buscar::clicked;Long		ll_fila
string 	ls_argume1, ls_argume2, ls_argume3, ls_busqueda

IF dw_1.RowCount() > 0 THEN
	ls_argume1 = sle_argumento4.text
	ls_argume2 = sle_argumento2.text
	ls_argume3 = sle_argumento3.text
	IF cbx_entrefechas.enabled THEN
		
		IF ls_argume3 = "" THEN
			sle_argumento3.text = sle_argumento4.text
			ls_argume3 = ls_argume1
		END IF
		
		IF not isdate(ls_argume1) or not isdate(ls_argume3) THEN
			Messagebox("Error", "Debe ingresar fechas validas", Exclamation!)
			RETURN
		END IF
		
		is_busca = "embq_fzarpe"
		
		IF cbx_entrefechas.checked THEN
			ls_busqueda = "embq_fzarpe between date('" + ls_argume1 + "') and date('" + ls_argume3 + "')"
		ELSE
			ls_busqueda = "embq_fzarpe = date('" + ls_argume1 + "')"
		END IF
		
	ELSE
		
		is_busca = "embq_codigo"
		ls_busqueda = "embq_codigo = '" + ls_argume2 + "'"
		
	END IF
	
	dw_1.SetSort(is_busca + " A")
	dw_1.Sort()
	tab_1.tabpage_2.dw_3.Reset()
	
	IF cbx_filtra.Checked THEN
		dw_1.SetFilter(ls_busqueda)
		ll_fila	= dw_1.Filter()
		
		IF ll_fila < 1 THEN
			Messagebox("Aviso", "No Existen datos para este criterio de busqueda", Information!)
			dw_1.SetFilter("")
			dw_1.Filter()
		END IF
		
	ELSE
		ll_fila	= dw_1.Find(ls_busqueda, 1, dw_1.RowCount())
		
		IF ll_fila = 0 THEN ll_fila = 1
		
		dw_1.SetRedraw(False)
		dw_1.SetRow(ll_fila)
		dw_1.ScrollToRow(ll_fila)
		dw_1.SelectRow(0,False)
		dw_1.SelectRow(ll_fila,True)
		dw_1.SetFocus()
		dw_1.SetRedraw(True)
	END IF
END IF
end event

type st_1 from statictext within tabpage_1
integer x = 192
integer y = 120
integer width = 306
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within tabpage_1
integer x = 530
integer y = 116
integer width = 1170
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;//datawindowchild dwc_planta

istr_busq.Argum[1] = string(data)
end event

type cbx_entrefechas from checkbox within tabpage_3
integer x = 1513
integer y = 292
integer width = 457
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 12632256
string text = "Entre Fechas"
end type

event clicked;if this.checked then
	sle_argumento3.visible = true
	st_y.visible = true
else
	sle_argumento3.visible = false
	st_y.visible = false
end if
end event

type st_y from statictext within tabpage_3
boolean visible = false
integer x = 1015
integer y = 264
integer width = 59
integer height = 120
boolean bringtotop = true
integer textsize = -20
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 12632256
string text = "-"
boolean focusrectangle = false
end type

type cbx_filtra from checkbox within tabpage_3
integer x = 1513
integer y = 180
integer width = 457
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 12632256
string text = "Filtra Datos"
end type

type sle_argumento3 from editmask within tabpage_3
integer x = 1079
integer y = 288
integer width = 402
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/YYYY"
end type

type sle_argumento4 from editmask within tabpage_3
integer x = 594
integer y = 288
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/YYYY"
end type

event getfocus;//
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
sle_argumento3.enabled = TRUE
st_y.enabled = TRUE
cbx_entrefechas.TriggerEvent("Clicked")
cbx_entrefechas.enabled = TRUE
end event

