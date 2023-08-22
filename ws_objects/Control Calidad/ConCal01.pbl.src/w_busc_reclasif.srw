$PBExportHeader$w_busc_reclasif.srw
$PBExportComments$búsqueda de Planillas de Lotes Objetados Pendientes.
forward
global type w_busc_reclasif from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
type pb_1 from picturebutton within tabpage_3
end type
end forward

global type w_busc_reclasif from w_busqueda
integer x = 123
integer y = 304
integer width = 2610
integer height = 1840
string title = "Búsqueda de Reclasificacion"
end type
global w_busc_reclasif w_busc_reclasif

type variables
String		is_TipoPallet, is_EstadoPallet, is_CondicPallet, is_Planta
DataWindowChild	idwc_planta

end variables

on w_busc_reclasif.create
int iCurrent
call super::create
end on

on w_busc_reclasif.destroy
call super::destroy
end on

event open;/*
	Argumentos	:	[1]	=	Cliente
*/
Long		ll_fila = 1
String	ls_Filtro


istr_busq.argum[3]	=	""
istr_busq.argum[4]	=	""
istr_busq.argum[5]	=	""
istr_busq.argum[6]	=	""
istr_busq.argum[7]	=	""


es_numero				=	True
istr_busq 				=	Message.PowerObjectParm
istr_busq.Argum[5]	=	""
is_Planta    			=  istr_busq.Argum[5]

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

tab_1.tabpage_1.dw_cliente.SetTransObject(sqlca)
tab_1.tabpage_1.dw_cliente.InsertRow(0)

is_ordena = 'Nro Reclasif:recl_numero,Planta:plde_codigo,Fecha:recl_fereet'

dw_1.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(Integer(istr_busq.argum[3]))



TriggerEvent("ue_ordenamiento")

IF istr_busq.argum[1] <> "" AND istr_busq.argum[5] <> "" THEN
	tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", Integer(istr_busq.argum[1]))
	tab_1.tabpage_1.dw_cliente.SetRow(ll_fila)
	tab_1.tabpage_1.dw_cliente.ScrollToRow(ll_fila)
	tab_1.tabpage_1.dw_cliente.SetFocus()
	
	tab_1.tabpage_1.pb_filtrar.TriggerEvent(Clicked!)

	IF dw_1.RowCount() > 0 THEN
		tab_1.tabpage_1.dw_cliente.Enabled = False
		dw_1.SetFocus()
	ELSE
		CloseWithReturn(This, istr_busq)
		Return
	END IF
ELSE
	tab_1.tabpage_1.dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)
	istr_busq.argum[1]	=	String(tab_1.tabpage_1.dw_cliente.GetItemNumber(1, "clie_codigo"))
   istr_busq.argum[5]	=	String(9999)
END IF
end event

event ue_asignacion;istr_Busq.Argum[1]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])//planta
istr_Busq.Argum[2]	= String(dw_1.Object.recl_numero[dw_1.GetRow()])//reclasif
istr_Busq.Argum[3]	= String(dw_1.Object.clie_codigo[dw_1.GetRow()])//cliente
istr_Busq.Argum[4]	= String(dw_1.Object.recl_fereet[dw_1.GetRow()])//fecha
istr_Busq.Argum[5]	= String(dw_1.Object.recl_observ[dw_1.GetRow()])//observ

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_reclasif
boolean visible = false
integer x = 2834
integer y = 1108
integer taborder = 0
end type

type dw_1 from w_busqueda`dw_1 within w_busc_reclasif
integer x = 59
integer y = 732
integer width = 2085
integer height = 868
integer taborder = 0
string dataobject = "dw_mues_reclasif_busq"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_reclasif
integer x = 2267
integer y = 1392
integer taborder = 20
alignment htextalign = center!
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_reclasif
integer x = 59
integer y = 96
integer width = 2094
long backcolor = 33554431
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
integer width = 2057
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
integer x = 1792
integer y = 296
integer taborder = 20
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;String		ls_Filtro

IF dw_1.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
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
integer width = 2057
long backcolor = 1073741824
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2057
string text = "Búsqueda          "
pb_1 pb_1
end type

on tabpage_3.create
this.pb_1=create pb_1
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_1
end on

on tabpage_3.destroy
call super::destroy
destroy(this.pb_1)
end on

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
integer x = 402
integer y = 312
fontcharset fontcharset = defaultcharset!
string facename = "Arial"
end type

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
boolean visible = false
integer x = 142
integer y = 320
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 942
integer y = 196
integer width = 517
fontcharset fontcharset = defaultcharset!
string facename = "Arial"
end type

event sle_argumento1::modified;is_argume	= This.Text
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 146
integer y = 204
integer width = 681
string text = "Número reclasificacion"
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
integer x = 123
integer y = 196
integer width = 315
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
integer x = 398
integer y = 188
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

type pb_1 from picturebutton within tabpage_3
integer x = 1829
integer y = 292
integer width = 155
integer height = 132
integer taborder = 25
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\buscae.bmp"
string disabledname = "\desarrollo\bmp\buscad.bmp"
alignment htextalign = left!
end type

event clicked;Long		ll_fila

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

