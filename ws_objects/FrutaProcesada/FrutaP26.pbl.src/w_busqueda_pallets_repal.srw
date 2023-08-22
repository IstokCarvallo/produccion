$PBExportHeader$w_busqueda_pallets_repal.srw
forward
global type w_busqueda_pallets_repal from w_busqueda
end type
type dw_planta from datawindow within tabpage_1
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
type st_3 from statictext within tabpage_1
end type
type em_repa from editmask within tabpage_1
end type
end forward

global type w_busqueda_pallets_repal from w_busqueda
integer width = 3182
integer height = 1896
string title = "Busca Repaletizajes"
end type
global w_busqueda_pallets_repal w_busqueda_pallets_repal

type variables
Integer ii_planta, ii_cliente
datawindowchild idwc_planta, idwc_cliente
end variables

on w_busqueda_pallets_repal.create
int iCurrent
call super::create
end on

on w_busqueda_pallets_repal.destroy
call super::destroy
end on

event open;call super::open;x=0
y=0

istr_busq.argum[1] = ""
istr_busq.argum[2] = ""

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

PostEvent("ue_ordenamiento")
is_ordena = 'Tipo Pallet:repa_tipopa,Fecha:repa_fecrep,Nro. Movimiento:repa_numero'

This.Icon	=	Gstr_apl.Icono




tab_1.tabpage_1.dw_planta.Getchild("plde_codigo",idwc_planta)
tab_1.tabpage_1.dw_cliente.Getchild("clie_codigo",idwc_cliente)

idwc_planta.SettransObject(sqlca)
idwc_cliente.SettransObject(sqlca)

idwc_planta.Retrieve()
idwc_cliente.Retrieve()

tab_1.tabpage_1.dw_Planta.InsertRow(0)
tab_1.tabpage_1.dw_cliente.InsertRow(0)

tab_1.tabpage_1.dw_Planta.SetItem(1,"plde_codigo",integer(istr_mant.argumento[2]))
tab_1.tabpage_1.dw_cliente.SetItem(1,"clie_codigo",integer(istr_mant.argumento[1]))

ii_planta = integer(istr_mant.argumento[2])
ii_cliente =integer(istr_mant.argumento[1])
tab_1.tabpage_1.em_repa.text = string(long(istr_mant.argumento[3]),"00000000")

IF istr_mant.argumento[3] = "" THEN
	istr_mant.argumento[3] = '-1'
END IF

IF dw_1.Retrieve(ii_planta,ii_cliente,long(istr_mant.argumento[3])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
   CLosewithreturn(this,istr_busq)
END IF
end event

event ue_asignacion;//str_busqueda lstr_busq

istr_busq.argum[1]=String(ii_cliente)     
istr_busq.argum[2]=String(ii_planta)
istr_busq.argum[3]=String(dw_1.object.paen_numero[dw_1.GetRow()])
CloseWithReturn(This, istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busqueda_pallets_repal
boolean visible = false
integer x = 2875
end type

type dw_1 from w_busqueda`dw_1 within w_busqueda_pallets_repal
integer width = 2688
string dataobject = "dw_mues_busq_pallets_repa"
end type

type pb_salir from w_busqueda`pb_salir within w_busqueda_pallets_repal
integer x = 2848
end type

event pb_salir::clicked;istr_busq.argum[1]=""
CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busqueda_pallets_repal
integer x = 82
integer width = 2679
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
integer width = 2642
dw_planta dw_planta
st_1 st_1
st_2 st_2
dw_cliente dw_cliente
st_3 st_3
em_repa em_repa
end type

on tabpage_1.create
this.dw_planta=create dw_planta
this.st_1=create st_1
this.st_2=create st_2
this.dw_cliente=create dw_cliente
this.st_3=create st_3
this.em_repa=create em_repa
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_planta
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.em_repa
end on

on tabpage_1.destroy
call super::destroy
destroy(this.dw_planta)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_cliente)
destroy(this.st_3)
destroy(this.em_repa)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1938
boolean enabled = false
end type

event pb_filtrar::clicked;
ii_planta = tab_1.tabpage_1.dw_planta.object.plde_codigo[1]
ii_cliente = tab_1.tabpage_1.dw_cliente.object.clie_codigo[1]

IF dw_1.Retrieve(ii_planta,ii_cliente) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)

END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2642
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2642
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 667
integer y = 240
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))

This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "repa_fecrep"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 146
integer y = 248
string text = "Fecha"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 667
integer y = 120
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "repa_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 146
integer y = 128
integer width = 462
string text = "Nº Repaletizaje"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
end type

type dw_planta from datawindow within tabpage_1
integer x = 718
integer y = 152
integer width = 1157
integer height = 108
integer taborder = 25
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
end type

type st_1 from statictext within tabpage_1
integer x = 210
integer y = 164
integer width = 206
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 210
integer y = 64
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
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within tabpage_1
integer x = 722
integer y = 44
integer width = 1157
integer height = 96
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

type st_3 from statictext within tabpage_1
integer x = 210
integer y = 268
integer width = 480
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
string text = "Nº Repalletizaje"
boolean focusrectangle = false
end type

type em_repa from editmask within tabpage_1
integer x = 727
integer y = 268
integer width = 485
integer height = 88
integer taborder = 61
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

