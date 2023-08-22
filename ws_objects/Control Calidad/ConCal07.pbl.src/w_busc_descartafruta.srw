$PBExportHeader$w_busc_descartafruta.srw
forward
global type w_busc_descartafruta from w_busqueda
end type
type st_2 from statictext within tabpage_1
end type
type st_1 from statictext within tabpage_1
end type
type uo_selcliente from uo_seleccion_clientesprod within tabpage_1
end type
type uo_selespecie from uo_seleccion_especie within tabpage_1
end type
end forward

global type w_busc_descartafruta from w_busqueda
integer x = 78
integer y = 176
integer width = 3305
integer height = 1832
string title = "Búsqueda Fruta de Descarte de Packing"
boolean resizable = false
end type
global w_busc_descartafruta w_busc_descartafruta

type variables
DataWindowChild	idwc_planta,idwc_prod,idwc_espe

Integer	ii_fila
Integer  ii_SwOrden[]	=	{0, 0, 0}
end variables

event open;Long		ll_fila = 1

x	=	30
y	=	280

istr_busq = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
dw_1.SetRowFocusIndicator(Hand!)

Tab_1.TabPage_1.uo_SelEspecie.Seleccion(False, False)
Tab_1.TabPage_1.uo_SelEspecie.Inicia(Long(istr_busq.argum[3]))

Tab_1.TabPage_1.uo_SelCliente.Seleccion(False, False)
Tab_1.TabPage_1.uo_SelCliente.Inicia(Long(istr_Busq.Argum[1]))

//Tab_1.TabPage_1.uo_SelEspecie.Bloquear(True)
//Tab_1.TabPage_1.uo_SelCliente.Bloquear(True)

is_ordena = 'Nº. Planill:cced_codigo,Zona:zona_codigo,Planta:plde_codigo,Fecha:cced_fecmov'

TriggerEvent("ue_ordenamiento")

ii_fila = 1

Tab_1.TabPage_1.pb_Filtrar.PostEvent(Clicked!)
end event

on w_busc_descartafruta.create
int iCurrent
call super::create
end on

on w_busc_descartafruta.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[1]	=	String(dw_1.Object.cced_numero[ii_fila])
istr_busq.argum[2]	=	String(dw_1.Object.zona_codigo[ii_fila])
istr_busq.argum[3]	=	String(dw_1.Object.plde_codigo[ii_fila])
istr_busq.argum[4]	=	String(dw_1.Object.clie_codigo[ii_fila])
istr_busq.argum[5]	=	String(dw_1.Object.cced_tipore[ii_fila])
istr_busq.argum[6]	=	String(dw_1.Object.cced_fecmov[ii_fila])


CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_descartafruta
boolean visible = false
integer x = 2665
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_descartafruta
integer y = 760
integer width = 2702
integer height = 900
integer taborder = 60
string dataobject = "dw_busc_ctlcaldescarteespecie"
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

event dw_1::clicked;call super::clicked;IF Row > 0 THEN
   ii_fila	=	Row
	This.SelectRow(0,False)
	This.SelectRow(Row,True)
	This.SetRow(Row)
END IF
end event

type pb_salir from w_busqueda`pb_salir within w_busc_descartafruta
integer x = 2912
integer y = 1400
end type

event pb_salir::clicked;istr_busq.argum[4]	=	""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_descartafruta
integer x = 69
integer y = 96
integer width = 2555
integer height = 628
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
integer width = 2519
integer height = 500
string text = "Filtros                         "
st_2 st_2
st_1 st_1
uo_selcliente uo_selcliente
uo_selespecie uo_selespecie
end type

on tabpage_1.create
this.st_2=create st_2
this.st_1=create st_1
this.uo_selcliente=create uo_selcliente
this.uo_selespecie=create uo_selespecie
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selespecie
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.uo_selcliente)
destroy(this.uo_selespecie)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2304
integer y = 292
end type

event pb_filtrar::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]),Integer(istr_busq.argum[3]), Integer(istr_busq.argum[4])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2519
integer height = 500
string text = "Ordenamiento              "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2304
integer taborder = 20
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer taborder = 30
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer taborder = 10
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
boolean visible = false
integer width = 2519
integer height = 500
boolean enabled = false
string text = "Búsqueda                      "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 704
integer y = 252
integer width = 411
fontcharset fontcharset = defaultcharset!
string facename = "Arial"
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))

This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "cclo_numero"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 133
integer y = 260
string text = "Nº. Planilla"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 704
integer y = 132
integer width = 411
fontcharset fontcharset = defaultcharset!
string facename = "Arial"
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "prod_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 133
integer y = 140
integer width = 521
string text = "Productor"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2309
integer y = 312
end type

type st_2 from statictext within tabpage_1
integer x = 343
integer y = 132
integer width = 274
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_1 from statictext within tabpage_1
integer x = 343
integer y = 288
integer width = 274
integer height = 76
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

type uo_selcliente from uo_seleccion_clientesprod within tabpage_1
integer x = 690
integer y = 284
integer height = 80
integer taborder = 25
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;istr_Busq.Argum[1] = String(This.Codigo)
end event

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selespecie from uo_seleccion_especie within tabpage_1
integer x = 690
integer y = 128
integer height = 80
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

