$PBExportHeader$w_busc_ctlcaltercerinspeccion.srw
$PBExportComments$búsqueda de Planillas de Lotes Objetados Pendientes.
forward
global type w_busc_ctlcaltercerinspeccion from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
type dw_especie from datawindow within tabpage_1
end type
type dw_planta from datawindow within tabpage_1
end type
type st_4 from statictext within tabpage_1
end type
type st_5 from statictext within tabpage_1
end type
type cbx_planta from checkbox within tabpage_1
end type
type st_2 from statictext within w_busc_ctlcaltercerinspeccion
end type
type st_3 from statictext within w_busc_ctlcaltercerinspeccion
end type
end forward

global type w_busc_ctlcaltercerinspeccion from w_busqueda
integer x = 123
integer y = 304
integer width = 3090
integer height = 2012
string title = "Búsqueda de Lotes Objetados Pendientes"
st_2 st_2
st_3 st_3
end type
global w_busc_ctlcaltercerinspeccion w_busc_ctlcaltercerinspeccion

type variables
String		is_TipoPallet, is_EstadoPallet, is_CondicPallet, is_Planta
DataWindowChild	idwc_planta

end variables

on w_busc_ctlcaltercerinspeccion.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_3=create st_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
end on

on w_busc_ctlcaltercerinspeccion.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_3)
end on

event open;call super::open;/*
	Argumentos	:	[1]	=	Cliente
	               [2]   =  Planta
						[7]   =  Especie
*/
Long		ll_fila = 1
Integer	ll_Null
String	ls_Filtro
Boolean  lb_cerrar 

SetNull(ll_Null)

istr_busq.argum[3]	=	""
istr_busq.argum[4]	=	""
istr_busq.argum[5]	=	""
istr_busq.argum[6]	=	""
istr_busq.argum[7]	=	""

es_numero				=	True
istr_busq 				=	Message.PowerObjectParm
istr_busq.Argum[5]	=	""
is_Planta    				=  istr_busq.Argum[5]

dw_1.SetTransObject(sqlca)
dw_1.SetRowFocusIndicator(Hand!)

tab_1.tabpage_1.dw_cliente.SetTransObject(sqlca)
tab_1.tabpage_1.dw_especie.SetTransObject(sqlca)
tab_1.tabpage_1.dw_planta.SetTransObject(sqlca)
tab_1.tabpage_1.dw_cliente.InsertRow(0)
tab_1.tabpage_1.dw_especie.InsertRow(0)
tab_1.tabpage_1.dw_planta.InsertRow(0)

is_ordena = 'Lote:cclo_numero,Nro. Resolucion:ccte_numero,Fecha:ccte_fecins'

//TriggerEvent("ue_ordenamiento")

IF istr_busq.argum[1] <> "" /*AND istr_busq.argum[5] <> ""*/ THEN
	tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", Integer(istr_busq.argum[1]))
	tab_1.tabpage_1.dw_cliente.SetRow(ll_fila)
	tab_1.tabpage_1.dw_cliente.ScrollToRow(ll_fila)
	tab_1.tabpage_1.dw_cliente.SetFocus()
	tab_1.tabpage_1.dw_especie.SetItem(1, "espe_codigo", Integer(istr_busq.argum[7]))
//	tab_1.tabpage_1.dw_planta.SetItem(1, "plde_codigo", Integer(istr_busq.argum[2]))
	istr_busq.argum[2]	= '9999'
	
	tab_1.tabpage_1.pb_filtrar.TriggerEvent(Clicked!)

	IF dw_1.RowCount() > 0 THEN
		tab_1.tabpage_1.dw_cliente.Enabled = False
		tab_1.tabpage_1.dw_especie.Enabled = False
		tab_1.tabpage_1.dw_planta.Enabled = False
		dw_1.SetFocus()
	ELSE
		CloseWithReturn(This, istr_busq)
		RETURN
	END IF
ELSE
	tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)
	tab_1.tabpage_1.dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)
	tab_1.tabpage_1.dw_planta.Enabled	=	False
	istr_busq.argum[1]	=	String(tab_1.tabpage_1.dw_cliente.GetItemNumber(1, "clie_codigo"))
   	istr_busq.argum[5]	=	String(9999)
	istr_busq.argum[2]	= '9999'
END IF

/* 
Realiza Retrieve 
*/

IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]),&
                 Integer(istr_busq.argum[7])) > 0 THEN
	dw_1.SetFIlter("")
	dw_1.Filter()

	dw_1.SetFilter(ls_Filtro)
	dw_1.Filter()
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	CloseWithReturn(This,istr_busq)
END IF
end event

event ue_asignacion;istr_Busq.Argum[1]	= String(dw_1.Object.ccte_numero[dw_1.GetRow()])
istr_Busq.Argum[2]	= String(dw_1.Object.zona_codigo[dw_1.GetRow()])
istr_Busq.Argum[3]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[4]	= String(dw_1.Object.clie_codigo[dw_1.GetRow()])
istr_Busq.Argum[5]	= String(dw_1.Object.cctc_codigo[dw_1.GetRow()])
istr_Busq.Argum[6]	= String(dw_1.Object.ccte_fecins[dw_1.GetRow()])
istr_Busq.Argum[7]   = String(dw_1.Object.espe_codigo[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_ctlcaltercerinspeccion
boolean visible = false
integer x = 2775
integer y = 1108
integer taborder = 0
end type

type dw_1 from w_busqueda`dw_1 within w_busc_ctlcaltercerinspeccion
integer x = 37
integer y = 732
integer width = 2642
integer height = 1128
integer taborder = 0
string dataobject = "dw_mues_tercerainpeccion"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_ctlcaltercerinspeccion
integer x = 2775
integer y = 1368
integer taborder = 20
alignment htextalign = center!
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_ctlcaltercerinspeccion
integer x = 110
integer y = 64
integer width = 2405
integer selectedtab = 9
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
integer width = 2368
string text = "Filtros              "
st_1 st_1
dw_cliente dw_cliente
dw_especie dw_especie
dw_planta dw_planta
st_4 st_4
st_5 st_5
cbx_planta cbx_planta
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.dw_especie=create dw_especie
this.dw_planta=create dw_planta
this.st_4=create st_4
this.st_5=create st_5
this.cbx_planta=create cbx_planta
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.dw_especie
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.cbx_planta
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.dw_especie)
destroy(this.dw_planta)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.cbx_planta)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2126
integer y = 296
integer taborder = 20
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;String		ls_Filtro

IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]),&
                 Integer(istr_busq.argum[7])) > 0 THEN
	dw_1.SetFIlter("")
	dw_1.Filter()

	dw_1.SetFilter(ls_Filtro)
	dw_1.Filter()
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2368
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2135
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2368
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
integer x = 690
integer y = 308
fontcharset fontcharset = defaultcharset!
string facename = "Arial"
end type

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
boolean visible = false
integer x = 96
integer y = 320
long textcolor = 0
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 690
integer y = 196
integer width = 402
fontcharset fontcharset = defaultcharset!
string facename = "Arial"
end type

event sle_argumento1::modified;is_argume	= This.Text
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 96
integer y = 204
integer width = 521
long textcolor = 0
string text = "Número de Pallet"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2459
integer y = 312
end type

event pb_buscar::clicked;Long		ll_fila

IF dw_1.RowCount() > 0 THEN
	dw_1.SetSort(is_busca + " A")
	dw_1.Sort()

	tab_1.tabpage_2.dw_3.Reset()
	
	IF es_numero THEN
		ll_fila	= dw_1.Find(is_busca + " >= " + is_argume, 1, dw_1.RowCount())
	ELSE
		is_busca	= "Mid(" + is_busca + ", 1, " + String(Len(is_argume)) + ")"
		ll_fila	= dw_1.Find(is_busca + " >= " + is_argume + "'" , 1, dw_1.RowCount())
	END IF

	IF ll_fila = 0 THEN ll_fila = 1
	
	dw_1.SetRedraw(False)
	dw_1.SetRow(ll_fila)
	dw_1.ScrollToRow(ll_fila)
	dw_1.SelectRow(0,False)
	dw_1.SelectRow(ll_fila,True)
	dw_1.SetFocus()
	dw_1.SetRedraw(True)
END IF
end event

type st_1 from statictext within tabpage_1
integer x = 288
integer y = 72
integer width = 274
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

type dw_cliente from datawindow within tabpage_1
integer x = 631
integer y = 56
integer width = 1143
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[1]=data
end event

type dw_especie from datawindow within tabpage_1
integer x = 631
integer y = 160
integer width = 910
integer height = 108
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

type dw_planta from datawindow within tabpage_1
integer x = 631
integer y = 336
integer width = 1193
integer height = 96
integer taborder = 25
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[2] = data
end event

type st_4 from statictext within tabpage_1
integer x = 288
integer y = 172
integer width = 279
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_5 from statictext within tabpage_1
integer x = 288
integer y = 336
integer width = 311
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
string text = "Frigorífico"
boolean focusrectangle = false
end type

type cbx_planta from checkbox within tabpage_1
integer x = 635
integer y = 256
integer width = 283
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_null)

IF This.Checked THEN
	dw_planta.Enabled	=	False
	dw_planta.SetItem(1,"plde_codigo", li_null)
   istr_busq.argum[2]	= '9999'
ELSE
	dw_planta.Enabled	=	True
END IF


end event

type st_2 from statictext within w_busc_ctlcaltercerinspeccion
integer x = 402
integer y = 440
integer width = 274
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type st_3 from statictext within w_busc_ctlcaltercerinspeccion
integer x = 407
integer y = 548
integer width = 274
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

