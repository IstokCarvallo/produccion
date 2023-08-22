$PBExportHeader$w_busc_spro_ordenproceso_traspaso.srw
$PBExportComments$Busca ordenes  para traspaso a proceso
forward
global type w_busc_spro_ordenproceso_traspaso from w_busqueda
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

global type w_busc_spro_ordenproceso_traspaso from w_busqueda
integer x = 123
integer y = 304
integer width = 4297
string title = "Busca Ordenes de Proceso"
boolean controlmenu = true
end type
global w_busc_spro_ordenproceso_traspaso w_busc_spro_ordenproceso_traspaso

type variables
datawindowchild  idwc_planta, idwc_productor, idwc_variedad
end variables

event open;call super::open;/*
Argumentos
istr_Busq.Argum[1]	:	Planta
istr_Busq.Argum[15]	:	Tipo Orden
istr_Busq.Argum[16]	:	Cliente
*/

istr_busq	=	Message.PowerObjectParm

is_ordena 	=	'Orden:orpr_numero,Productor:prod_codigo,Especie:espe_codigo'

tab_1.tabpage_1.dw_5.GetChild("plde_codigo", idwc_planta)

idwc_planta.SetTransObject(SqlCa)

IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
END IF

tab_1.tabpage_1.dw_4.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(SqlCa)
idwc_productor.Retrieve(-1)
tab_1.tabpage_1.dw_4.SetTransObject(SqlCa)
tab_1.tabpage_1.dw_4.insertrow(0)

tab_1.tabpage_1.dw_6.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(SqlCa)
idwc_variedad.Retrieve(gstr_ParamPlanta.Codigoespecie)
tab_1.tabpage_1.dw_6.SetTransObject(SqlCa)
tab_1.tabpage_1.dw_6.insertrow(0)

tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.InsertRow(0)

IF istr_Busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_Busq.Argum[1]))
	tab_1.tabpage_1.dw_5.Enabled	=	False
END IF

istr_busq.Argum[3]	=	"4"

tab_1.tabpage_1.pb_filtrar.SetFocus()
end event

on w_busc_spro_ordenproceso_traspaso.create
int iCurrent
call super::create
end on

on w_busc_spro_ordenproceso_traspaso.destroy
call super::destroy
end on

event ue_asignacion;//istr_Busq.Argum[3]	=	String(dw_1.Object.plde_codigo[dw_1.GetRow()])
//istr_Busq.Argum[4]	=	String(dw_1.Object.espe_codigo[dw_1.GetRow()])
//istr_Busq.Argum[5]	=	String(dw_1.Object.orpr_fecpro[dw_1.GetRow()])
//istr_Busq.Argum[6]	=	String(dw_1.Object.orpr_numero[dw_1.GetRow()])
//istr_Busq.Argum[7]	=	String(dw_1.Object.prod_codigo[dw_1.GetRow()])
//istr_Busq.Argum[8]	=	String(dw_1.Object.vari_codigo[dw_1.GetRow()])
//istr_busq.argum[1]   = "0"
istr_Busq.Argum[1]	=	String(dw_1.Object.orpr_numero[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_ordenproceso_traspaso
boolean visible = false
integer x = 3927
integer y = 1040
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_ordenproceso_traspaso
integer width = 3611
string dataobject = "dw_mues_ordenesproceso_traspaso"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_spro_ordenproceso_traspaso
integer x = 3922
integer y = 1316
end type

event pb_salir::clicked;istr_busq.Argum[1] = "0"
CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_spro_ordenproceso_traspaso
integer x = 119
integer y = 72
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
integer x = 1966
integer y = 284
long backcolor = 134217742
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

IF dw_1.Retrieve(li_planta,ll_productor,li_variedad,Integer(istr_Busq.Argum[16]), Integer(istr_busq.Argum[3])) > 0 THEN
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
end type

event sle_argumento2::modified;call super::modified;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "prod_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
long backcolor = 16777215
string text = "Productor"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
end type

event sle_argumento1::modified;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0

es_numero						= True
is_busca							= "orpr_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
long backcolor = 16777215
string text = "Orden Proc."
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1888
integer y = 256
long backcolor = 134217748
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
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
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
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within tabpage_1
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
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type dw_4 from datawindow within tabpage_1
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
integer x = 558
integer y = 336
integer width = 1088
integer height = 104
integer taborder = 61
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
end type

