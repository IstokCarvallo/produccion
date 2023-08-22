$PBExportHeader$w_busc_ctlcalplacualinspuvaenc.srw
$PBExportComments$Busqueda de Planilla Cualitativas
forward
global type w_busc_ctlcalplacualinspuvaenc from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
end forward

global type w_busc_ctlcalplacualinspuvaenc from w_busqueda
integer x = 123
integer y = 304
integer width = 3182
string title = "Búsqueda de Planillas"
long backcolor = 33543637
end type
global w_busc_ctlcalplacualinspuvaenc w_busc_ctlcalplacualinspuvaenc

type variables
DataWindowChild	idwc_planta, idwc_productor, idwc_especie

end variables

on w_busc_ctlcalplacualinspuvaenc.create
int iCurrent
call super::create
end on

on w_busc_ctlcalplacualinspuvaenc.destroy
call super::destroy
end on

event open;call super::open;/*
	Argumentos	:	[1]	=	Cliente
*/

Long		ll_Fila = 1

es_numero				=	True
istr_Busq 				=	Message.PowerObjectParm

istr_Busq.Argum[2]	=	""

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

tab_1.tabpage_1.dw_cliente.SetTransObject(sqlca)
tab_1.tabpage_1.dw_cliente.InsertRow(0)

is_ordena = 'Número Folio:ccce_numero,Planta:plde_codigo,Número Lote:cclo_numero'

TriggerEvent("ue_ordenamiento")

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(gi_CodExport)

dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(SQLCA)
idwc_productor.Retrieve(gi_CodExport)

dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve(gi_CodExport)

IF istr_Busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", Integer(istr_Busq.Argum[1]))
	tab_1.tabpage_1.dw_cliente.SetRow(ll_fila)
	tab_1.tabpage_1.dw_cliente.ScrollToRow(ll_fila)
	tab_1.tabpage_1.dw_cliente.SetFocus()

	tab_1.tabpage_1.pb_filtrar.TriggerEvent(Clicked!)

	IF dw_1.RowCount() > 0 THEN
		tab_1.tabpage_1.dw_cliente.Enabled = False
		dw_1.SetFocus()
	ELSE
		CloseWithReturn(This, istr_Busq)
		RETURN
	END IF
ELSE
	tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)
	istr_Busq.Argum[1]	=	String(tab_1.tabpage_1.dw_cliente.GetItemNumber(1, "clie_codigo"))
END IF
end event

event ue_asignacion();istr_Busq.Argum[1]	= String(dw_1.Object.clie_codigo[dw_1.GetRow()])
istr_Busq.Argum[2]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[3]	= String(dw_1.Object.ccce_numero[dw_1.GetRow()])
istr_Busq.Argum[4]	= String(dw_1.Object.cclo_numero[dw_1.GetRow()])
istr_Busq.Argum[5]	= String(dw_1.Object.zona_codigo[dw_1.GetRow()])
istr_Busq.Argum[6]	= String(dw_1.Object.prod_codigo[dw_1.GetRow()])
istr_Busq.Argum[7]	= String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_Busq.Argum[8]	= String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_Busq.Argum[9]	= dw_1.Object.emba_codigo[dw_1.GetRow()]
istr_Busq.Argum[10]	= dw_1.Object.vaca_calibr[dw_1.GetRow()]
istr_Busq.Argum[11]	= String(dw_1.Object.plde_codpak[dw_1.GetRow()])
istr_Busq.Argum[12]	= String(dw_1.Object.cclo_fecemb[dw_1.GetRow()])
istr_Busq.Argum[13]	= String(dw_1.Object.cclo_tamlot[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_ctlcalplacualinspuvaenc
boolean visible = false
integer x = 2834
integer y = 1108
integer taborder = 50
long backcolor = 33543637
end type

type dw_1 from w_busqueda`dw_1 within w_busc_ctlcalplacualinspuvaenc
integer x = 37
integer y = 732
integer width = 2697
integer taborder = 60
string dataobject = "dw_mues_ctlcalplacualinspuvaenc"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_ctlcalplacualinspuvaenc
integer x = 2834
integer y = 1368
alignment htextalign = center!
long backcolor = 33543637
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_ctlcalplacualinspuvaenc
integer x = 37
integer y = 92
integer width = 2693
long backcolor = 33543637
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2656
long backcolor = 33543637
string text = "Filtros              "
long tabbackcolor = 12632256
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
integer x = 2432
integer y = 312
integer width = 215
integer height = 184
integer taborder = 20
boolean default = false
long backcolor = 33543637
end type

event pb_filtrar::clicked;call super::clicked;String	ls_Filtro

IF dw_1.Retrieve(Integer(istr_Busq.Argum[1])) > 0 THEN
	dw_1.SetFilter("")
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
integer width = 2656
long backcolor = 33543637
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2432
integer y = 312
long backcolor = 33543637
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2656
long backcolor = 33543637
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
integer x = 713
integer y = 312
long backcolor = 30586022
end type

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
boolean visible = false
integer x = 96
integer y = 320
long backcolor = 33543637
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 713
integer y = 196
integer width = 402
end type

event sle_argumento1::modified;is_argume	= This.Text
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 96
integer y = 204
integer width = 576
long backcolor = 33543637
string text = "Número de Planilla"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2432
integer y = 312
integer width = 215
integer height = 184
long backcolor = 33543637
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
		ll_fila	= dw_1.Find(is_busca + " >= '" + is_argume + "'", 1, dw_1.RowCount())
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
integer x = 78
integer y = 192
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within tabpage_1
integer x = 421
integer y = 180
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

