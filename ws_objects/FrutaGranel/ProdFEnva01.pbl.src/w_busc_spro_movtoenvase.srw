$PBExportHeader$w_busc_spro_movtoenvase.srw
forward
global type w_busc_spro_movtoenvase from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_6 from datawindow within tabpage_1
end type
type dw_4 from datawindow within tabpage_1
end type
type dw_5 from datawindow within tabpage_1
end type
type st_4 from statictext within tabpage_1
end type
type st_3 from st_argum2 within tabpage_3
end type
type sle_argumento3 from sle_argumento2 within tabpage_3
end type
end forward

global type w_busc_spro_movtoenvase from w_busqueda
integer x = 123
integer y = 304
integer width = 3081
integer height = 2104
string title = "Búsqueda de Movimiento de Fruta"
end type
global w_busc_spro_movtoenvase w_busc_spro_movtoenvase

type variables
datawindowchild  idwc_tipomov, idwc_planta, idwc_cliente


end variables

on w_busc_spro_movtoenvase.create
int iCurrent
call super::create
end on

on w_busc_spro_movtoenvase.destroy
call super::destroy
end on

event open;call super::open;Date	ld_fecha
/*
	Argumentos
		istr_Busq.Argum[1]	:	Planta
		istr_Busq.Argum[2]	:	Tipo de Movimiento
		istr_Busq.Argum[3]	:	Estado del Movimiento
		istr_Busq.Argum[4]	:	Fecha de Inicio de Búsqueda
*/

istr_busq	=	Message.PowerObjectParm
is_ordena 	=	'Número:spro_movtoenvaenca_meen_numero,Fecha Movto.:spro_movtoenvaenca_meen_fecmov'

tab_1.tabpage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)

IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

tab_1.tabpage_1.dw_6.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SqlCa)

IF idwc_cliente.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Clientes")
	idwc_planta.InsertRow(0)
END IF

tab_1.tabpage_1.dw_4.GetChild("tpmv_codigo", idwc_tipomov)
idwc_tipomov.SetTransObject(sqlca)

IF idwc_tipomov.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipo de Movimiento")
	idwc_tipomov.InsertRow(0)
ELSE
	idwc_tipomov.SetFilter("tpmv_frugra=0 and tpmv_fruemb=0 and tpmv_frucom=0 and tpmv_envase=1 ")
	idwc_tipomov.Filter()
	idwc_tipomov.SetSort("tpmv_nombre A")
	idwc_tipomov.Sort()
END IF

tab_1.tabpage_1.dw_4.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_6.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_4.InsertRow(0)
tab_1.tabpage_1.dw_5.InsertRow(0)
tab_1.tabpage_1.dw_6.InsertRow(0)

IF istr_Busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_4.SetItem(1, "tpmv_codigo", Integer(istr_Busq.Argum[2]))
	tab_1.tabpage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_Busq.Argum[1]))
	tab_1.tabpage_1.dw_6.SetItem(1, "clie_codigo", Integer(istr_Busq.Argum[33]))
	
	tab_1.tabpage_1.dw_5.Enabled	=	False
	tab_1.tabpage_1.dw_6.Enabled	=	False
	
	istr_Mant.Argumento[1]	=	istr_Busq.Argum[1]
	istr_Mant.Argumento[2]	=	istr_Busq.Argum[2]
	tab_1.tabpage_1.pb_filtrar.PostEvent(Clicked!)
END IF


tab_1.tabpage_1.pb_filtrar.SetFocus()
end event

event ue_asignacion;//istr_Busq.Argum[1]	= 	istr_Mant.Argumento[1]
//istr_Busq.Argum[2]	=  istr_Mant.Argumento[2]
istr_Busq.Argum[3]	= 	String(dw_1.Object.meen_numero[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_movtoenvase
boolean visible = false
integer x = 2665
integer y = 1016
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_movtoenvase
integer x = 101
integer y = 764
integer width = 2459
integer height = 840
integer taborder = 60
string dataobject = "dw_busc_spro_movtoenvase"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_spro_movtoenvase
integer x = 2665
integer y = 1276
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_spro_movtoenvase
integer x = 101
integer y = 80
integer width = 2592
integer height = 632
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
integer width = 2555
integer height = 504
string text = "Filtros              "
st_1 st_1
st_2 st_2
dw_6 dw_6
dw_4 dw_4
dw_5 dw_5
st_4 st_4
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.dw_6=create dw_6
this.dw_4=create dw_4
this.dw_5=create dw_5
this.st_4=create st_4
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_6
this.Control[iCurrent+4]=this.dw_4
this.Control[iCurrent+5]=this.dw_5
this.Control[iCurrent+6]=this.st_4
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_6)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.st_4)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2217
integer y = 248
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;
IF dw_1.Retrieve(Integer(istr_Busq.Argum[1]),Integer(istr_Busq.Argum[2]),Integer(istr_Busq.Argum[33])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2555
integer height = 504
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1760
integer y = 312
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
integer height = 440
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
integer height = 440
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2555
integer height = 504
string text = "Búsqueda          "
st_3 st_3
sle_argumento3 sle_argumento3
end type

on tabpage_3.create
this.st_3=create st_3
this.sle_argumento3=create sle_argumento3
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.sle_argumento3
end on

on tabpage_3.destroy
call super::destroy
destroy(this.st_3)
destroy(this.sle_argumento3)
end on

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 645
integer y = 168
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
integer y = 176
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 645
integer y = 48
integer width = 306
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= True
is_busca							= "spro_movtoenvaenca_meen_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer y = 56
integer width = 389
string text = "Número Lote"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2226
integer y = 248
end type

event pb_buscar::clicked;Long		ll_fila
Boolean	lb_inicio, lb_termino

dw_1.SetRedraw(False)

IF dw_1.RowCount() > 0 THEN
	dw_1.SetSort(is_busca + " A")
	dw_1.Sort()
	dw_1.SetFilter("")
	dw_1.Filter()

	tab_1.tabpage_2.dw_3.Reset()
	
	IF es_numero THEN
		ll_fila	=	dw_1.Find(is_busca + " >= " + is_argume, 1, dw_1.RowCount())
	ELSE
		IF Mid(is_argume, 1, 1) = "%" THEN
			lb_inicio	=	True
			is_argume	=	Mid(is_argume, 2)
		END IF
		
		IF Mid(is_argume, Len(is_argume)) = "%" THEN
			lb_termino	=	True
			is_argume	=	Mid(is_argume, 1, Len(is_argume) - 1)
		END IF
		
		IF Not lb_inicio THEN
			is_busca	=	"Mid(String(" + is_busca + "), 1, " + String(Len(is_argume)) + ")"
			ll_fila	=	dw_1.Find(is_busca + " >= '" + is_argume + "'", 1, dw_1.RowCount())
		ELSEIF lb_inicio THEN
			Is_busca	=	"Pos(Lower(String(" + is_busca + ")), '" + Lower(is_argume) + "') > 0"
			dw_1.SetFilter(Is_busca)
			dw_1.Filter()
		END IF
	END IF

	IF ll_fila = 0 THEN ll_fila = 1
	
	dw_1.SetRow(ll_fila)
	dw_1.ScrollToRow(ll_fila)
	dw_1.SelectRow(0,False)
	dw_1.SelectRow(ll_fila,True)
	dw_1.SetFocus()
	dw_1.SetRedraw(True)
END IF
end event

type st_1 from statictext within tabpage_1
integer x = 64
integer y = 220
integer width = 288
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 64
integer y = 340
integer width = 352
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Tipo Movto."
boolean focusrectangle = false
end type

type dw_6 from datawindow within tabpage_1
integer x = 489
integer y = 100
integer width = 1166
integer height = 96
integer taborder = 41
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "clie_codigo"
		istr_Busq.Argum[33]	=	data
END CHOOSE
end event

type dw_4 from datawindow within tabpage_1
integer x = 489
integer y = 324
integer width = 873
integer height = 96
integer taborder = 41
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_spro_tipomovtofruta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "tpmv_codigo"
		istr_Busq.Argum[2]	=	data
END CHOOSE
end event

type dw_5 from datawindow within tabpage_1
integer x = 489
integer y = 212
integer width = 873
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "plde_codigo"
		istr_Busq.Argum[1]	=	data
END CHOOSE
end event

type st_4 from statictext within tabpage_1
integer x = 64
integer y = 108
integer width = 288
integer height = 84
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

type st_3 from st_argum2 within tabpage_3
integer y = 296
boolean bringtotop = true
string text = "Productor"
end type

type sle_argumento3 from sle_argumento2 within tabpage_3
integer y = 288
integer taborder = 25
boolean bringtotop = true
end type

event getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= False
is_busca							= "prod_nombre"
end event

