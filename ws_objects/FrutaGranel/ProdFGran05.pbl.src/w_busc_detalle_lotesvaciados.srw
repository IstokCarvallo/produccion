$PBExportHeader$w_busc_detalle_lotesvaciados.srw
forward
global type w_busc_detalle_lotesvaciados from w_busqueda
end type
type dw_5 from datawindow within tabpage_1
end type
type st_1 from statictext within tabpage_1
end type
end forward

global type w_busc_detalle_lotesvaciados from w_busqueda
integer x = 123
integer y = 304
integer width = 2976
string title = "Busca Ordenes de Proceso"
end type
global w_busc_detalle_lotesvaciados w_busc_detalle_lotesvaciados

type variables
datawindowchild  idwc_planta
end variables

event open;call super::open;/*
Argumentos
istr_Busq.Argum[1]	:	Planta
istr_Busq.Argum[2]	:	TipoMovto
istr_Busq.Argum[3]	:	Numero
istr_Busq.Argum[4]	:	Cliente

*/

istr_busq	=	Message.PowerObjectParm
is_ordena 	=	'Nº Lote:lote,Tipo envase:tiposenvases_tien_nombre,Envase:envases_enva_nombre'

tab_1.tabpage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)

IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
END IF

tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.InsertRow(0)

IF istr_Busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_Busq.Argum[1]))
	tab_1.tabpage_1.dw_5.Enabled	=	False
END IF

IF dw_1.Retrieve(Integer(istr_Busq.Argum[1]),Integer(istr_Busq.Argum[2]),+ &
					  Integer(istr_Busq.Argum[3]),Integer(istr_Busq.Argum[4])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	istr_Busq.Argum[1]=""
	CloseWithReturn(this, istr_busq)
END IF
end event

on w_busc_detalle_lotesvaciados.create
int iCurrent
call super::create
end on

on w_busc_detalle_lotesvaciados.destroy
call super::destroy
end on

event ue_asignacion();
istr_Busq.Argum[4]	=	String(dw_1.Object.lote[dw_1.GetRow()])
istr_Busq.Argum[5]	=	String(dw_1.Object.enva_tipoen[dw_1.GetRow()])
istr_Busq.Argum[6]	=	String(dw_1.Object.enva_codigo[dw_1.GetRow()])
istr_Busq.Argum[7]	=	String(dw_1.Object.envases_enva_nombre[dw_1.GetRow()])
istr_Busq.Argum[8]	=	String(dw_1.Object.mfgd_bulent[dw_1.GetRow()])
istr_Busq.Argum[9]	=	String(dw_1.Object.lote_pltcod[dw_1.GetRow()])
istr_Busq.Argum[10]	=	String(dw_1.Object.lote_espcod[dw_1.GetRow()])
istr_Busq.Argum[11]	=	String(dw_1.Object.lote_codigo[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_detalle_lotesvaciados
boolean visible = false
integer x = 2665
integer y = 1120
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_detalle_lotesvaciados
integer x = 73
integer width = 2464
string dataobject = "dw_busc_detalle_lotesvaciados"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_detalle_lotesvaciados
integer x = 2665
integer y = 1424
end type

event pb_salir::clicked;istr_Busq.Argum[1]=""

CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_detalle_lotesvaciados
integer x = 73
integer width = 2464
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
integer width = 2427
dw_5 dw_5
st_1 st_1
end type

on tabpage_1.create
this.dw_5=create dw_5
this.st_1=create st_1
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_5
this.Control[iCurrent+2]=this.st_1
end on

on tabpage_1.destroy
call super::destroy
destroy(this.dw_5)
destroy(this.st_1)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2053
integer y = 324
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_Busq.Argum[1]),Integer(istr_Busq.Argum[2]),+ &
					  Integer(istr_Busq.Argum[3]),Integer(istr_Busq.Argum[4])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2427
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2427
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
boolean enabled = false
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
boolean visible = false
string text = "Productor"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 571
integer y = 164
integer width = 736
end type

event sle_argumento1::modified;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0

es_numero						= False
is_busca							= "lote"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 155
integer y = 172
integer width = 439
string text = "Lote"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
end type

type dw_5 from datawindow within tabpage_1
integer x = 745
integer y = 172
integer width = 873
integer height = 96
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
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
integer x = 338
integer y = 180
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
boolean disabledlook = true
end type

