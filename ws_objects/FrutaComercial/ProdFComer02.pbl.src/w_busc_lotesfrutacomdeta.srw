$PBExportHeader$w_busc_lotesfrutacomdeta.srw
$PBExportComments$Ventana de Busqueda de Lotes Fruta Comercial.
forward
global type w_busc_lotesfrutacomdeta from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_4 from datawindow within tabpage_1
end type
type dw_5 from datawindow within tabpage_1
end type
type st_3 from st_argum2 within tabpage_3
end type
type sle_argumento3 from sle_argumento2 within tabpage_3
end type
end forward

global type w_busc_lotesfrutacomdeta from w_busqueda
integer x = 123
integer y = 304
integer width = 3328
string title = "Búsqueda de Lotes"
end type
global w_busc_lotesfrutacomdeta w_busc_lotesfrutacomdeta

type variables
datawindowchild  idwc_especie, idwc_planta


end variables

on w_busc_lotesfrutacomdeta.create
int iCurrent
call super::create
end on

on w_busc_lotesfrutacomdeta.destroy
call super::destroy
end on

event open;call super::open;istr_busq	=	Message.PowerObjectParm

istr_busq.Argum[13]	=	""

is_ordena = 'Número Lote:lofc_lotefc,Productor:prod_nombre,' + &
				'Grupo Calibre:lofc_grucal'

tab_1.tabpage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_especie.InsertRow(0)
END IF

tab_1.tabpage_1.dw_4.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

tab_1.tabpage_1.dw_4.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_4.InsertRow(0)
tab_1.tabpage_1.dw_5.InsertRow(0)

IF istr_busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_5.SetItem(1,"plde_codigo",Integer(istr_busq.Argum[1]))
END IF

IF istr_busq.Argum[2] <> "" THEN
	tab_1.tabpage_1.dw_4.SetItem(1,"espe_codigo",Integer(istr_busq.Argum[2]))
END IF

tab_1.tabpage_1.pb_filtrar.triggerevent("clicked")

tab_1.tabpage_1.pb_filtrar.SetFocus()
end event

event ue_asignacion();istr_busq.argum[1]	= String(dw_1.Object.lofc_pltcod[dw_1.GetRow()])
istr_busq.argum[2]	= String(dw_1.Object.lofc_espcod[dw_1.GetRow()])
istr_busq.argum[3]	= String(dw_1.Object.lofc_lotefc[dw_1.GetRow()])
istr_busq.argum[4]	= String(dw_1.Object.lfcd_secuen[dw_1.GetRow()])
istr_busq.argum[5]	= String(dw_1.Object.prod_codigo[dw_1.GetRow()])
istr_busq.argum[6]	= dw_1.Object.prod_nombre[dw_1.GetRow()]


CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_lotesfrutacomdeta
boolean visible = false
integer x = 2866
integer y = 864
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_lotesfrutacomdeta
integer x = 82
integer y = 740
integer width = 2720
integer taborder = 60
string dataobject = "dw_mues_lotesfrutacomdeta"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_lotesfrutacomdeta
integer x = 2903
end type

event pb_salir::clicked;istr_busq.argum[1] = ""
istr_busq.argum[2] = ""
istr_busq.argum[3] = ""
istr_busq.argum[4] = ""
istr_busq.argum[5] = ""
istr_busq.argum[6] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_lotesfrutacomdeta
integer x = 101
integer y = 80
integer width = 2176
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
integer width = 2139
string text = "Filtros              "
st_1 st_1
st_2 st_2
dw_4 dw_4
dw_5 dw_5
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.dw_4=create dw_4
this.dw_5=create dw_5
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.dw_5
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_4)
destroy(this.dw_5)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1577
integer y = 312
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1]), Integer(istr_busq.argum[2])) > 0 THEN

	dw_1.SetFilter("prod_codigo = "+ istr_busq.argum[4] + " And vari_codigo = "+istr_busq.argum[3])
	dw_1.Filter()

	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2139
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
integer width = 2139
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
is_busca							= "lofc_lotefc"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer width = 389
string text = "Número Lote"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1760
integer y = 300
end type

type st_1 from statictext within tabpage_1
integer x = 64
integer y = 88
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

type st_2 from statictext within tabpage_1
integer x = 55
integer y = 244
integer width = 256
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type dw_4 from datawindow within tabpage_1
integer x = 384
integer y = 220
integer width = 873
integer height = 96
integer taborder = 31
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "espe_codigo"
		istr_busq.Argum[2]	=	data
END CHOOSE
end event

type dw_5 from datawindow within tabpage_1
integer x = 384
integer y = 76
integer width = 873
integer height = 96
integer taborder = 20
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
		istr_busq.Argum[1]	=	data
END CHOOSE
end event

type st_3 from st_argum2 within tabpage_3
integer y = 328
boolean bringtotop = true
string text = "Categoria"
end type

type sle_argumento3 from sle_argumento2 within tabpage_3
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

