$PBExportHeader$w_busc_orden_proceso.srw
forward
global type w_busc_orden_proceso from w_busqueda
end type
type dw_5 from datawindow within tabpage_1
end type
type st_1 from statictext within tabpage_1
end type
end forward

global type w_busc_orden_proceso from w_busqueda
integer x = 123
integer y = 304
integer width = 3113
string title = "Busca Ordenes de Proceso"
end type
global w_busc_orden_proceso w_busc_orden_proceso

type variables
datawindowchild  idwc_planta

integer il_tipord=4
end variables

event open;call super::open;/*
Argumentos	:	
		[1]	=>	Planta
		[6]	=>	Tipo de Orden
*/

istr_busq				=	Message.PowerObjectParm
is_ordena 				=	'Orden:orpr_numero,Productor:prod_codigo,' + &
								'Especie:espe_codigo'
//istr_Busq.Argum[4]	=	''

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
ELSE
	tab_1.tabpage_1.dw_5.SetItem(1, "plde_codigo",gstr_paramplanta.codigoplanta)
	istr_Busq.Argum[1] = string(gstr_paramplanta.codigoplanta)
END IF

IF istr_busq.Argum[6] = '' THEN
	il_tipord	=	4
ELSE	
	il_tipord	=	Integer(istr_busq.Argum[6])
END IF	

istr_busq.Argum[6]	=	""

tab_1.tabpage_1.pb_filtrar.SetFocus()

IF dw_1.Retrieve(Integer(istr_Busq.Argum[1]), 0, il_tipord,Integer(istr_Busq.Argum[4])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
END IF
end event

on w_busc_orden_proceso.create
int iCurrent
call super::create
end on

on w_busc_orden_proceso.destroy
call super::destroy
end on

event ue_asignacion();istr_Busq.Argum[1]	=	String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[2]	=	String(dw_1.Object.orpr_numero[dw_1.GetRow()])
istr_Busq.Argum[3]	=	String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_Busq.Argum[4]	=	String(dw_1.Object.prod_codigo[dw_1.GetRow()])
istr_Busq.Argum[5]	=	String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_Busq.Argum[6]	=	'4'
istr_Busq.Argum[7]	=	String(dw_1.Object.orpr_niveld[dw_1.GetRow()])
istr_Busq.Argum[8]	=	String(dw_1.Object.orpr_fecpro[dw_1.GetRow()],"dd/mm/yyyy")
istr_Busq.Argum[9]	=	String(dw_1.Object.orpr_canbul[dw_1.GetRow()])
istr_Busq.Argum[10]	=	String(dw_1.Object.orpr_tipord[dw_1.GetRow()])
istr_Busq.Argum[11]	=	String(dw_1.Object.ppre_numero[dw_1.GetRow()])
istr_Busq.Argum[12]	=	String(dw_1.Object.line_codigo[dw_1.GetRow()])
istr_Busq.Argum[13]	=	String(dw_1.Object.orpr_nrotur[dw_1.GetRow()])
istr_Busq.Argum[14]	=	dw_1.Object.frio_tipofr[dw_1.GetRow()]
istr_Busq.Argum[15]	=	String(dw_1.Object.pefr_codigo[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_orden_proceso
boolean visible = false
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_orden_proceso
integer width = 2633
string dataobject = "dw_mues_ordenesproceso"
end type

event dw_1::doubleclicked;IF row > 0 THEN
	Parent.PostEvent("ue_asignacion")
END IF
end event

type pb_salir from w_busqueda`pb_salir within w_busc_orden_proceso
integer x = 2834
integer y = 1412
end type

event pb_salir::clicked;istr_busq.argum[1] = ""
istr_busq.argum[2] = ""
istr_busq.argum[3] = ""

CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_orden_proceso
integer x = 73
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
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
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_Busq.Argum[1]), 0, il_tipord,Integer(istr_Busq.Argum[4])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
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
string text = "Orden Proc."
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
end type

type dw_5 from datawindow within tabpage_1
integer x = 635
integer y = 92
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
integer x = 229
integer y = 100
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

