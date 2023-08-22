$PBExportHeader$w_busc_spro_salidaspacking.srw
$PBExportComments$Busca ordenes  para traspaso a proceso
forward
global type w_busc_spro_salidaspacking from w_busqueda
end type
type dw_5 from datawindow within tabpage_1
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type st_3 from statictext within tabpage_1
end type
type dw_4 from datawindow within tabpage_1
end type
type dw_6 from datawindow within tabpage_1
end type
end forward

global type w_busc_spro_salidaspacking from w_busqueda
integer x = 123
integer y = 304
integer width = 3145
string title = "Busca Ordenes de Proceso"
end type
global w_busc_spro_salidaspacking w_busc_spro_salidaspacking

type variables
datawindowchild  idwc_planta, idwc_productor, idwc_variedad
end variables

event open;call super::open;/*
Argumentos
*/

istr_busq	=	Message.PowerObjectParm

is_ordena 	=	'Salida:lisa_codigo,Nombre Salida:lisa_descri'

dw_1.Retrieve(Long(istr_busq.Argum[1]), Integer(istr_busq.Argum[2]))
end event

on w_busc_spro_salidaspacking.create
int iCurrent
call super::create
end on

on w_busc_spro_salidaspacking.destroy
call super::destroy
end on

event ue_asignacion;istr_Busq.Argum[1]	=	String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[2]	=	String(dw_1.Object.line_codigo[dw_1.GetRow()])
istr_Busq.Argum[3]	=	String(dw_1.Object.lisa_codigo[dw_1.GetRow()])
istr_Busq.Argum[4]	=	String(dw_1.Object.lisa_descri[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_salidaspacking
boolean visible = false
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_salidaspacking
integer width = 1605
string title = "dw_mues_entidades_bloq"
string dataobject = "dw_mues_salidalineapacking_muestra"
end type

event dw_1::doubleclicked;IF row > 0 THEN
	Parent.PostEvent("ue_asignacion")
END IF
end event

type pb_salir from w_busqueda`pb_salir within w_busc_spro_salidaspacking
integer x = 2775
integer y = 1428
end type

event pb_salir::clicked;istr_Busq.Argum[1]	=	String(0)
istr_Busq.Argum[2]	=	String(0)
istr_Busq.Argum[3]	=	String(0)
istr_Busq.Argum[4]	=	String(0)

CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_spro_salidaspacking
integer x = 73
integer y = 72
integer selectedtab = 2
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
boolean enabled = false
dw_5 dw_5
st_1 st_1
st_2 st_2
st_3 st_3
dw_4 dw_4
dw_6 dw_6
end type

on tabpage_1.create
this.dw_5=create dw_5
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.dw_4=create dw_4
this.dw_6=create dw_6
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_5
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.dw_4
this.Control[iCurrent+6]=this.dw_6
end on

on tabpage_1.destroy
call super::destroy
destroy(this.dw_5)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.dw_4)
destroy(this.dw_6)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
end type

event pb_filtrar::clicked;call super::clicked;Integer  li_variedad, li_planta, li_null
Long     ll_productor

SetNull(li_null)

istr_busq.argum[1] = "0"

li_planta=tab_1.tabpage_1.dw_5.Object.plde_codigo[1]

IF tab_1.tabpage_1.dw_4.Object.prod_codigo[1]=0 or IsNull(tab_1.tabpage_1.dw_4.Object.prod_codigo[1]) THEN
	ll_productor=0
ELSE
	ll_productor=tab_1.tabpage_1.dw_4.Object.prod_codigo[1]
END IF	

IF tab_1.tabpage_1.dw_6.Object.vari_codigo[1]=0 or IsNull(tab_1.tabpage_1.dw_6.Object.vari_codigo[1]) THEN
	li_variedad=0
ELSE
	li_variedad=tab_1.tabpage_1.dw_6.Object.vari_codigo[1]
END IF

IF dw_1.Retrieve(li_planta,ll_productor,li_variedad,Integer(istr_Busq.Argum[16]), integer(Istr_Busq.Argum[15])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	tab_1.tabpage_1.dw_4.setitem(1,'prod_codigo',li_null)
	tab_1.tabpage_1.dw_6.setitem(1,'vari_codigo',li_null)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 539
end type

event sle_argumento2::modified;call super::modified;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "pers_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 544
end type

event sle_argumento1::modified;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0

es_numero						= False
is_busca							= "pers_apepat"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer width = 503
string text = "Tarjeta"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
end type

type dw_5 from datawindow within tabpage_1
integer x = 558
integer y = 116
integer width = 873
integer height = 96
integer taborder = 10
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

type st_1 from statictext within tabpage_1
integer x = 151
integer y = 124
integer width = 288
integer height = 84
boolean bringtotop = true
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

type st_2 from statictext within tabpage_1
boolean visible = false
integer x = 151
integer y = 236
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within tabpage_1
boolean visible = false
integer x = 151
integer y = 344
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type dw_4 from datawindow within tabpage_1
boolean visible = false
integer x = 558
integer y = 232
integer width = 919
integer height = 100
integer taborder = 25
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
end type

type dw_6 from datawindow within tabpage_1
boolean visible = false
integer x = 558
integer y = 340
integer width = 914
integer height = 104
integer taborder = 61
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
end type

