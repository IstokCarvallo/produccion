$PBExportHeader$w_busqueda_spro_repalasigenca.srw
forward
global type w_busqueda_spro_repalasigenca from w_busqueda
end type
type dw_planta from datawindow within tabpage_1
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
end forward

global type w_busqueda_spro_repalasigenca from w_busqueda
integer width = 3182
integer height = 1896
string title = "Busca Repaletizajes"
end type
global w_busqueda_spro_repalasigenca w_busqueda_spro_repalasigenca

type variables
Integer ii_planta, ii_cliente
datawindowchild idwc_planta, idwc_cliente
end variables

on w_busqueda_spro_repalasigenca.create
int iCurrent
call super::create
end on

on w_busqueda_spro_repalasigenca.destroy
call super::destroy
end on

event open;call super::open;x=0
y=0

istr_busq.argum[1] = ""
istr_busq.argum[2] = ""

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

tab_1.tabpage_1.dw_Planta.SetItem(1,"plde_codigo",gi_CodPlanta)
tab_1.tabpage_1.dw_cliente.SetItem(1,"clie_codigo",gi_CodExport)

ii_planta = gi_CodPlanta
ii_cliente =gi_CodExport

IF dw_1.Retrieve(ii_planta,ii_cliente) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
  // CLosewithreturn(this,istr_busq)
END IF
end event

event ue_asignacion;//str_busqueda lstr_busq

istr_busq.argum[1]=String(ii_cliente)     
istr_busq.argum[2]=String(ii_planta)
istr_busq.argum[3]=String(dw_1.object.repe_numero[dw_1.GetRow()])
CloseWithReturn(This, istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busqueda_spro_repalasigenca
boolean visible = false
integer x = 2875
end type

type dw_1 from w_busqueda`dw_1 within w_busqueda_spro_repalasigenca
integer width = 2688
string dataobject = "dw_mues_busq_repalasienca"
end type

type pb_salir from w_busqueda`pb_salir within w_busqueda_spro_repalasigenca
integer x = 2848
end type

event pb_salir::clicked;istr_busq.argum[1]=""
CloseWithReturn(Parent, istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busqueda_spro_repalasigenca
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
end type

on tabpage_1.create
this.dw_planta=create dw_planta
this.st_1=create st_1
this.st_2=create st_2
this.dw_cliente=create dw_cliente
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_planta
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_cliente
end on

on tabpage_1.destroy
call super::destroy
destroy(this.dw_planta)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_cliente)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1938
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
integer y = 264
integer width = 1170
integer height = 108
integer taborder = 25
boolean bringtotop = true
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
end type

type st_1 from statictext within tabpage_1
integer x = 370
integer y = 280
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
long backcolor = 16777215
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 370
integer y = 176
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
long backcolor = 16777215
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within tabpage_1
integer x = 718
integer y = 156
integer width = 1147
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

