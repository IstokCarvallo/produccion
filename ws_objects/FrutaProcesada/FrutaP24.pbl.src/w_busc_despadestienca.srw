$PBExportHeader$w_busc_despadestienca.srw
forward
global type w_busc_despadestienca from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
type dw_planta from datawindow within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type cbx_entrefechas from checkbox within tabpage_3
end type
type st_y from statictext within tabpage_3
end type
type sle_argumento3 from singlelineedit within tabpage_3
end type
type cbx_filtra from checkbox within tabpage_3
end type
end forward

global type w_busc_despadestienca from w_busqueda
integer x = 123
integer y = 304
integer width = 2857
string title = "Búsqueda de Pallets"
end type
global w_busc_despadestienca w_busc_despadestienca

type variables
String		is_TipoPallet, is_EstadoPallet, is_CondicPallet, is_Planta
DataWindowChild	idwc_planta

end variables

on w_busc_despadestienca.create
int iCurrent
call super::create
end on

on w_busc_despadestienca.destroy
call super::destroy
end on

event ue_asignacion;
istr_busq.argum[5]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"dese_numero"))
istr_busq.argum[6]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"clie_codigo"))
istr_busq.argum[7]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"plde_codigo"))


CloseWithReturn(This,istr_busq)
end event

event open;call super::open;Datawindowchild dwc_planta
istr_busq 				=	Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()

Tab_1.TabPage_1.dw_cliente.SetTransObject(sqlca)
Tab_1.TabPage_1.dw_cliente.InsertRow(0)
Tab_1.TabPage_1.dw_cliente.object.clie_codigo[1] = integer(istr_busq.Argum[1])
Tab_1.TabPage_1.dw_planta.GetChild("plde_codigo", dwc_planta)
Tab_1.TabPage_1.dw_planta.InsertRow(0)
dwc_planta.SetTransObject(sqlca)
dwc_planta.Retrieve()
Tab_1.TabPage_1.dw_planta.SetItem(1, "plde_codigo", integer(istr_busq.Argum[2]))

is_ordena = "Cliente:clie_codigo,Planta:plde_codigo,Nro Proceso:dese_numero,Codigo Embq:embq_codigo,Fecha de Proceso:dese_fecsel"
end event

event resize;call super::resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_despadestienca
boolean visible = false
integer y = 1332
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_despadestienca
integer x = 37
integer y = 712
integer taborder = 60
string dataobject = "dw_busca_despadestienca"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_despadestienca
integer y = 1036
alignment htextalign = center!
end type

event pb_salir::clicked;istr_busq.argum[5] = ""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_despadestienca
integer x = 37
integer y = 92
integer width = 2350
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
integer width = 2313
string text = "Filtros              "
st_1 st_1
dw_cliente dw_cliente
dw_planta dw_planta
st_2 st_2
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.dw_planta=create dw_planta
this.st_2=create st_2
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.dw_planta
this.Control[iCurrent+4]=this.st_2
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.dw_planta)
destroy(this.st_2)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2071
integer y = 304
integer taborder = 20
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;String		ls_Filtro

dw_1.Retrieve(integer(istr_busq.Argum[1]), integer(istr_busq.Argum[2]), integer(istr_busq.Argum[10]))
IF dw_1.RowCount() > 0 then	
//
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2313
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
integer width = 2313
string text = "Búsqueda          "
cbx_entrefechas cbx_entrefechas
st_y st_y
sle_argumento3 sle_argumento3
cbx_filtra cbx_filtra
end type

on tabpage_3.create
this.cbx_entrefechas=create cbx_entrefechas
this.st_y=create st_y
this.sle_argumento3=create sle_argumento3
this.cbx_filtra=create cbx_filtra
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cbx_entrefechas
this.Control[iCurrent+2]=this.st_y
this.Control[iCurrent+3]=this.sle_argumento3
this.Control[iCurrent+4]=this.cbx_filtra
end on

on tabpage_3.destroy
call super::destroy
destroy(this.cbx_entrefechas)
destroy(this.st_y)
destroy(this.sle_argumento3)
destroy(this.cbx_filtra)
end on

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 594
integer y = 128
integer width = 402
long backcolor = 1073741824
end type

event sle_argumento2::getfocus;call super::getfocus;//
sle_argumento3.enabled = false
st_y.enabled = false
cbx_entrefechas.TriggerEvent("Clicked")
cbx_entrefechas.enabled = false
//
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer y = 132
integer width = 535
integer height = 88
boolean enabled = true
string text = "Codigo Embarque"
alignment alignment = right!
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 594
integer y = 264
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
integer y = 272
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
	ls_argume1 = sle_argumento1.text
	ls_argume2 = sle_argumento2.text
	ls_argume3 = sle_argumento3.text
	IF cbx_entrefechas.enabled THEN
		
		IF ls_argume3 = "" THEN
			sle_argumento3.text = sle_argumento1.text
			ls_argume3 = ls_argume1
		END IF
		
		IF not isdate(ls_argume1) or not isdate(ls_argume3) THEN
			Messagebox("Error", "Debe ingresar fechas validas", Exclamation!)
			RETURN
		END IF
		
		is_busca = "dese_fecsel"
		
		IF cbx_entrefechas.checked THEN
			ls_busqueda = "dese_fecsel between date('" + ls_argume1 + "') and date('" + ls_argume3 + "')"
		ELSE
			ls_busqueda = "dese_fecsel = date('" + ls_argume1 + "')"
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
integer x = 151
integer y = 136
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Cliente"
end type

type dw_cliente from datawindow within tabpage_1
integer x = 562
integer y = 124
integer width = 1157
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;datawindowchild dwc_planta
istr_busq.Argum[1] = string(data)
tab_1.tabpage_1.dw_planta.GetChild("plde_codigo", dwc_planta)
dwc_planta.SetTransObject(sqlca)
dwc_planta.Retrieve()
tab_1.tabpage_1.dw_planta.SetItem(1, "plde_codigo", dwc_planta.GetItemNumber(1, "plde_codigo"))

end event

type dw_planta from datawindow within tabpage_1
integer x = 562
integer y = 260
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantaclie"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.Argum[2] = string(data)
end event

type st_2 from statictext within tabpage_1
integer x = 151
integer y = 268
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Planta"
end type

type cbx_entrefechas from checkbox within tabpage_3
integer x = 1477
integer y = 268
integer width = 457
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
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
integer x = 1001
integer y = 236
integer width = 59
integer height = 120
boolean bringtotop = true
integer textsize = -20
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "-"
boolean focusrectangle = false
end type

type sle_argumento3 from singlelineedit within tabpage_3
boolean visible = false
integer x = 1061
integer y = 264
integer width = 402
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
end type

type cbx_filtra from checkbox within tabpage_3
integer x = 1477
integer y = 132
integer width = 457
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Filtra Datos"
end type

