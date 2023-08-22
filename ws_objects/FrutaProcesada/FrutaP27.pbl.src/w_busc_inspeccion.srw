$PBExportHeader$w_busc_inspeccion.srw
forward
global type w_busc_inspeccion from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_plantas from datawindow within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
end forward

global type w_busc_inspeccion from w_busqueda
integer x = 78
integer y = 176
integer width = 2688
integer height = 1808
string title = "Búsqueda de Inspecciones"
boolean resizable = false
end type
global w_busc_inspeccion w_busc_inspeccion

type variables
DataWindowChild	idwc_planta
end variables

event open;Long		ll_fila = 1
Integer	li_estado
istr_busq = Message.PowerObjectParm

istr_busq.argum[5]	= ""
istr_busq.argum[6]	= ""
istr_busq.argum[4]	= ""

li_estado = Integer(istr_busq.argum[3])

tab_1.tabpage_1.dw_plantas.GetChild("plde_codigo", idwc_planta)

idwc_planta.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

idwc_planta.Retrieve(1)

tab_1.tabpage_1.dw_plantas.SetTransObject(sqlca)
tab_1.tabpage_1.dw_plantas.InsertRow(0)
tab_1.tabpage_1.dw_cliente.SetTransObject(sqlca)
tab_1.tabpage_1.dw_cliente.InsertRow(0)

is_ordena = 'Número:inpe_numero,Fecha Inspección:inpe_fechai'

TriggerEvent("ue_ordenamiento")

IF UpperBound(istr_busq.argum) > 0 THEN
	IF istr_busq.argum[2] <> "" THEN
		idwc_planta.Retrieve(1)
		tab_1.tabpage_1.dw_plantas.SetItem(1, "plde_codigo", Integer(istr_busq.argum[2]))
		tab_1.tabpage_1.dw_plantas.SetRow(ll_fila)
		tab_1.tabpage_1.dw_plantas.ScrollToRow(ll_fila)
		tab_1.tabpage_1.dw_plantas.SetFocus()

		tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", Integer(istr_busq.argum[1]))
		tab_1.tabpage_1.dw_cliente.SetRow(ll_fila)
		tab_1.tabpage_1.dw_cliente.ScrollToRow(ll_fila)
		tab_1.tabpage_1.dw_cliente.SetFocus()	
		
		IF dw_1.Retrieve(Integer(istr_busq.argum[1]), Integer(istr_busq.argum[2]),li_estado) > 0 THEN
			tab_1.tabpage_1.dw_plantas.Enabled = False
			tab_1.tabpage_1.dw_cliente.Enabled = False
			dw_1.SetFocus()
			dw_1.SelectRow(1,True)
		ELSE
			MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
			CloseWithReturn(This, istr_busq)
			Return
		END IF
	ELSE
		tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", Integer(istr_busq.argum[1]))
		tab_1.tabpage_1.dw_plantas.SetItem(1, "plde_codigo", Integer(istr_busq.argum[2]))
		istr_busq.argum[2]	=	String(tab_1.tabpage_1.dw_plantas.GetItemNumber(1, "plde_codigo"))
		istr_busq.argum[1]	=	String(tab_1.tabpage_1.dw_cliente.GetItemNumber(1, "clie_codigo"))
	END IF
ELSE
	tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", Integer(istr_busq.argum[1]))
	tab_1.tabpage_1.dw_plantas.SetItem(1, "plde_codigo", Integer(istr_busq.argum[2]))
	istr_busq.argum[2]	=	String(tab_1.tabpage_1.dw_plantas.GetItemNumber(1, "plde_codigo"))
	istr_busq.argum[1]	=	String(tab_1.tabpage_1.dw_cliente.GetItemNumber(1, "clie_codigo"))
END IF
end event

on w_busc_inspeccion.create
int iCurrent
call super::create
end on

on w_busc_inspeccion.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[5]	= String(dw_1.Object.inpe_numero[dw_1.GetRow()])
istr_busq.argum[10]	= String(dw_1.Object.inpe_fechai[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_inspeccion
boolean visible = false
integer x = 2254
integer y = 1116
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_inspeccion
integer y = 760
integer width = 2048
integer taborder = 60
string dataobject = "dw_mues_buscinspecciones"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_inspeccion
integer x = 2254
integer y = 1400
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_inspeccion
integer x = 87
integer y = 64
integer width = 2071
integer height = 620
end type

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
integer width = 2034
integer height = 492
string text = "Filtros                         "
st_1 st_1
dw_plantas dw_plantas
dw_cliente dw_cliente
st_2 st_2
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_plantas=create dw_plantas
this.dw_cliente=create dw_cliente
this.st_2=create st_2
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_plantas
this.Control[iCurrent+3]=this.dw_cliente
this.Control[iCurrent+4]=this.st_2
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_plantas)
destroy(this.dw_cliente)
destroy(this.st_2)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2309
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1]), Integer(istr_busq.argum[2])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)

//	istr_busq.argum[5]	= String(dw_1.GetItemNumber(dw_1.GetRow(), "fumi_numero"))

ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	IF tab_1.tabpage_1.dw_plantas.Enabled THEN
		tab_1.tabpage_1.dw_plantas.SetFocus()
	END IF
END IF


end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2034
integer height = 492
string text = "Ordenamiento              "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2304
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2034
integer height = 492
string text = "Búsqueda                      "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 549
integer y = 252
integer width = 411
boolean enabled = false
end type

event sle_argumento2::getfocus;call super::getfocus;This.SelectText(1, Len(This.Text))

This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "rfpe_fecrec"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 133
integer y = 260
string text = "Fecha"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 549
integer y = 132
integer width = 411
end type

event sle_argumento1::getfocus;call super::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "rfpe_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 133
integer y = 144
string text = "Número"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2309
integer y = 312
end type

type st_1 from statictext within tabpage_1
integer x = 105
integer y = 300
integer width = 448
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
boolean enabled = false
string text = "Planta Emisión"
boolean focusrectangle = false
end type

type dw_plantas from datawindow within tabpage_1
integer x = 626
integer y = 300
integer width = 965
integer height = 96
integer taborder = 11
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[2] = data
istr_busq.argum[4] = idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
end event

type dw_cliente from datawindow within tabpage_1
integer x = 631
integer y = 120
integer width = 1225
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[1] = data
idwc_planta.Retrieve(1)
end event

type st_2 from statictext within tabpage_1
integer x = 110
integer y = 120
integer width = 402
integer height = 64
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

