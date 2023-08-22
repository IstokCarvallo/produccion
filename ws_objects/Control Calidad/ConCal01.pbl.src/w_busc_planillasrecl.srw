$PBExportHeader$w_busc_planillasrecl.srw
$PBExportComments$búsqueda de Planillas de Lotes Objetados Pendientes.
forward
global type w_busc_planillasrecl from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
end forward

global type w_busc_planillasrecl from w_busqueda
integer x = 123
integer y = 304
integer width = 3218
integer height = 1888
string title = "Búsqueda de Planillas"
long backcolor = 553648127
end type
global w_busc_planillasrecl w_busc_planillasrecl

type variables
String	is_Planta
DataWindowChild	idwc_planta

end variables

on w_busc_planillasrecl.create
int iCurrent
call super::create
end on

on w_busc_planillasrecl.destroy
call super::destroy
end on

event open;Long		ll_fila = 1
String	ls_Filtro


istr_busq.argum[3]	=	""
istr_busq.argum[4]	=	""
istr_busq.argum[5]	=	""
istr_busq.argum[6]	=	""
istr_busq.argum[7]	=	""
istr_busq.argum[8]	=	""


es_numero				=	True
istr_busq 				=	Message.PowerObjectParm
is_Planta    			=  istr_busq.Argum[2]

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

is_ordena = 'Numero Planilla:cctd_folpla,Planta:plde_codigo'

TriggerEvent("ue_ordenamiento")

dw_1.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(Integer(istr_busq.argum[1]))

/* Realiza Retrieve */

IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2])) > 0 THEN
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

event ue_asignacion;istr_Busq.Argum[3]	= String(dw_1.Object.cclo_numero[dw_1.GetRow()])//lote
istr_Busq.Argum[5]	= String(dw_1.Object.cctd_folpla[dw_1.GetRow()])// planilla
istr_Busq.Argum[6]	= String(dw_1.Object.vaca_calibr[dw_1.GetRow()])//calibre ant
istr_Busq.Argum[7]	= String(dw_1.Object.emba_codigo[dw_1.GetRow()])//embalaje ant
istr_Busq.Argum[4]	= String(dw_1.Object.vari_codigo[dw_1.GetRow()])//variedad

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_planillasrecl
boolean visible = false
integer x = 2862
integer y = 1124
integer taborder = 0
end type

type dw_1 from w_busqueda`dw_1 within w_busc_planillasrecl
integer x = 37
integer y = 732
integer width = 2697
integer taborder = 0
string dataobject = "dw_mues_planillas_clasif"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_planillasrecl
integer x = 2862
integer y = 1384
integer taborder = 20
alignment htextalign = center!
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_planillasrecl
integer x = 27
integer y = 72
integer width = 2693
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
boolean visible = false
integer width = 2656
boolean enabled = false
long backcolor = 33554431
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
integer x = 2395
integer y = 288
integer taborder = 20
boolean default = false
long textcolor = 33554431
end type

event pb_filtrar::clicked;call super::clicked;String		ls_Filtro

IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2])) > 0 THEN
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
integer width = 2656
long backcolor = 33554431
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1861
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2656
long backcolor = 33554431
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
integer x = 393
integer y = 312
fontcharset fontcharset = defaultcharset!
string facename = "Arial"
end type

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
boolean visible = false
integer y = 320
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 690
integer y = 196
integer width = 402
fontcharset fontcharset = defaultcharset!
string facename = "Arial"
end type

event sle_argumento1::modified;This.SelectText(1, Len(This.Text))
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 96
integer y = 204
integer width = 576
string text = "Número de Planilla"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2459
integer y = 312
end type

event pb_buscar::clicked;call super::clicked;//Long		ll_fila
//
//IF dw_1.RowCount() > 0 THEN
//	dw_1.SetSort(is_busca + " A")
//	dw_1.Sort()
//
//	tab_1.tabpage_2.dw_3.Reset()
//	
//	IF es_numero THEN
//		ll_fila	= dw_1.Find(is_busca + " >= " +is_argume, 1, dw_1.RowCount())
//		
//	ELSE
//		is_busca	= "Mid(" + is_busca + ", 1, " + String(Len(is_argume)) + ")"
//		ll_fila	= dw_1.Find(is_busca + " >= " + is_argume + "'" , 1, dw_1.RowCount())
//	END IF
//
//	IF ll_fila = 0 THEN ll_fila = 1
//	
//	dw_1.SetRedraw(False)
//	dw_1.SetRow(ll_fila)
//	dw_1.ScrollToRow(ll_fila)
//	dw_1.SelectRow(0,False)
//	dw_1.SelectRow(ll_fila,True)
//	dw_1.SetFocus()
//	dw_1.SetRedraw(True)
//END IF
end event

type st_1 from statictext within tabpage_1
integer x = 343
integer y = 136
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

type dw_cliente from datawindow within tabpage_1
integer x = 686
integer y = 136
integer width = 1143
integer height = 92
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[1]=data
end event

