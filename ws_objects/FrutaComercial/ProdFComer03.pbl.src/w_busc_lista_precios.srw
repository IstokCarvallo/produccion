$PBExportHeader$w_busc_lista_precios.srw
$PBExportComments$Ventana de Busqueda de Lotes Fruta Comercial.
forward
global type w_busc_lista_precios from w_busqueda
end type
type st_2 from statictext within tabpage_1
end type
type dw_4 from datawindow within tabpage_1
end type
type cbx_todas from checkbox within tabpage_1
end type
end forward

global type w_busc_lista_precios from w_busqueda
integer x = 123
integer y = 304
integer width = 3387
string title = "Búsqueda de Lotes"
end type
global w_busc_lista_precios w_busc_lista_precios

type variables
datawindowchild  idwc_especie


end variables

on w_busc_lista_precios.create
int iCurrent
call super::create
end on

on w_busc_lista_precios.destroy
call super::destroy
end on

event open;call super::open;
istr_busq.Argum[6]	=	""

istr_busq	=	Message.PowerObjectParm

is_ordena = 'Especie:espe_codigo,Variedad:vari_codigo,Categoria:cate_codigo'

tab_1.tabpage_1.dw_4.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

tab_1.tabpage_1.dw_4.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_4.InsertRow(0)

IF istr_busq.Argum[2] <> "0" THEN
	tab_1.tabpage_1.dw_4.SetItem(1,"espe_codigo",Integer(istr_busq.Argum[2]))
	tab_1.tabpage_1.dw_4.Enabled 			= TRUE
	tab_1.tabpage_1.cbx_todas.Checked	= FALSE
ELSE
	tab_1.tabpage_1.cbx_todas.Checked	= TRUE
	tab_1.tabpage_1.dw_4.Enabled			= FALSE
END IF

tab_1.tabpage_1.pb_filtrar.triggerevent("clicked")
end event

event ue_asignacion();istr_busq.argum[1]	= String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_busq.argum[2]	= String(dw_1.Object.grva_codigo[dw_1.GetRow()])
istr_busq.argum[3]	= String(dw_1.Object.grva_codsub[dw_1.GetRow()])
istr_busq.argum[4]	= String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_busq.argum[5]	= String(dw_1.Object.cate_codigo[dw_1.GetRow()])
istr_busq.argum[6]	= dw_1.Object.refe_gcalib[dw_1.GetRow()]
istr_busq.argum[7]	= String(dw_1.Object.tafc_preuni[dw_1.GetRow()])


CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_lista_precios
boolean visible = false
integer x = 2903
integer y = 1068
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_lista_precios
integer x = 82
integer y = 740
integer width = 2693
integer taborder = 60
string dataobject = "dw_busc_spro_tarifafrutacomercial"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_lista_precios
integer x = 2903
end type

event pb_salir::clicked;istr_busq.argum[1] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_lista_precios
integer x = 142
integer y = 88
integer width = 2107
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
integer width = 2071
string text = "Filtros              "
st_2 st_2
dw_4 dw_4
cbx_todas cbx_todas
end type

on tabpage_1.create
this.st_2=create st_2
this.dw_4=create dw_4
this.cbx_todas=create cbx_todas
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.cbx_todas
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_2)
destroy(this.dw_4)
destroy(this.cbx_todas)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1847
integer y = 332
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;date   ldt_fecha

IF istr_busq.argum[2]<>"" THEN
	
	ldt_fecha= date(istr_busq.argum[1])
	IF dw_1.Retrieve(ldt_fecha, Integer(istr_busq.argum[2])) > 0 THEN
	
		dw_1.SetFocus()
		dw_1.SelectRow(1,True)
	ELSE
		MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	END IF
ELSE
	MessageBox("Atención","Falta Seleccionar una o Todas las Especies",Exclamation!,Ok!)
END IF	
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2071
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1810
integer y = 292
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2071
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 649
integer y = 248
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "cate_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
integer y = 256
string text = "Categoria"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 645
integer y = 128
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer y = 136
integer width = 389
string text = "Variedad"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1806
integer y = 292
end type

type st_2 from statictext within tabpage_1
integer x = 82
integer y = 196
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
integer x = 411
integer y = 172
integer width = 873
integer height = 96
integer taborder = 31
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;
istr_busq.Argum[2]	=	data

end event

type cbx_todas from checkbox within tabpage_1
integer x = 1330
integer y = 176
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Todas"
end type

event clicked;Integer li_Null

SetNull(li_Null)

IF this.Checked THEN
	istr_busq.Argum[2] = "0"
	tab_1.tabpage_1.dw_4.Enabled 			= FALSE
	tab_1.tabpage_1.dw_4.SetItem(1,"espe_codigo",li_Null)
ELSE
	tab_1.tabpage_1.dw_4.Enabled			= TRUE
	istr_busq.Argum[2] = ""
END IF
end event

