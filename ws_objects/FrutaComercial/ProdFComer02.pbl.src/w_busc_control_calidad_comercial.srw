$PBExportHeader$w_busc_control_calidad_comercial.srw
$PBExportComments$Ventana de Busqueda de Control de Calidad Fruta Comercial.
forward
global type w_busc_control_calidad_comercial from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_5 from datawindow within tabpage_1
end type
type st_3 from st_argum2 within tabpage_3
end type
type sle_argumento3 from sle_argumento2 within tabpage_3
end type
end forward

global type w_busc_control_calidad_comercial from w_busqueda
integer x = 123
integer y = 304
integer width = 3227
string title = "Búsqueda de Lotes"
end type
global w_busc_control_calidad_comercial w_busc_control_calidad_comercial

type variables
datawindowchild  idwc_especie, idwc_planta


end variables

on w_busc_control_calidad_comercial.create
int iCurrent
call super::create
end on

on w_busc_control_calidad_comercial.destroy
call super::destroy
end on

event open;call super::open;istr_busq	=	Message.PowerObjectParm


is_ordena = 'Folio:ccco_folio,Productor:prod_nombre,' + &
				'especie:espe_nombre'

tab_1.tabpage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_especie.InsertRow(0)
END IF

tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.InsertRow(0)

IF istr_busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_5.SetItem(1,"plde_codigo",Integer(istr_busq.Argum[1]))
END IF

tab_1.tabpage_1.pb_filtrar.triggerevent("clicked")
tab_1.tabpage_1.pb_filtrar.SetFocus()

end event

event ue_asignacion();istr_busq.argum[2]	= String(dw_1.Object.ccco_folio[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_control_calidad_comercial
boolean visible = false
integer x = 2601
integer y = 1068
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_control_calidad_comercial
integer x = 82
integer y = 740
integer width = 2770
integer taborder = 60
string dataobject = "dw_mues_control_calidad_comercial"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_control_calidad_comercial
integer x = 2903
end type

event pb_salir::clicked;istr_busq.argum[1] = ""
istr_busq.argum[2] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_control_calidad_comercial
integer x = 101
integer y = 80
integer width = 2103
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
integer width = 2066
string text = "Filtros              "
st_1 st_1
dw_5 dw_5
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_5=create dw_5
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_5
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_5)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1577
integer y = 312
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2066
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1760
integer y = 312
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2066
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
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= rgb(166,180,210)
sle_argumento1.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= rgb(166,180,210)
sle_argumento3.TabOrder		= 0
es_numero						= False
is_busca							= "prod_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
string text = "Productor"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 645
integer width = 306
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= rgb(166,180,210)
sle_argumento2.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= rgb(166,180,210)
sle_argumento3.TabOrder		= 0
es_numero						= True
is_busca							= "ccco_folio"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer width = 407
string text = "Número Folio"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1760
integer y = 300
end type

type st_1 from statictext within tabpage_1
integer x = 206
integer y = 216
integer width = 288
integer height = 84
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
boolean focusrectangle = false
end type

type dw_5 from datawindow within tabpage_1
integer x = 526
integer y = 204
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
		istr_busq.Argum[1]	=	data
END CHOOSE
end event

type st_3 from st_argum2 within tabpage_3
boolean visible = false
integer y = 328
boolean bringtotop = true
string text = "Categoria"
end type

type sle_argumento3 from sle_argumento2 within tabpage_3
boolean visible = false
integer y = 320
integer taborder = 25
boolean bringtotop = true
end type

event getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= rgb(166,180,210)
sle_argumento1.TabOrder		= 0
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= rgb(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "cate_codigo"
end event

