$PBExportHeader$w_busc_agrupavariedades.srw
forward
global type w_busc_agrupavariedades from w_busqueda
end type
type dw_especies from datawindow within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
end forward

global type w_busc_agrupavariedades from w_busqueda
integer x = 78
integer y = 176
integer width = 3008
integer height = 1832
string title = "Búsqueda de Planillas Recepción"
boolean resizable = false
end type
global w_busc_agrupavariedades w_busc_agrupavariedades

type variables
DataWindowChild	idwc_planta,idwc_productor,idwc_espe,idwc_variedades

Integer	ii_fila
Integer  ii_SwOrden[]	=	{0, 0, 0}
end variables

event open;Long		ll_fila = 1

x	=	30
y	=	280

Tab_1.TabPage_1.dw_especies.Object.espe_codigo.protect	=	1
//Tab_1.TabPage_1.dw_especies.Object.espe_codigo.BackGround.Color	=	553648127

istr_Busq = Message.PowerObjectParm

Tab_1.TabPage_1.dw_especies.GetChild("espe_codigo", idwc_espe)
idwc_espe.SetTransObject(sqlca)
idwc_espe.Retrieve()

dw_1.SetTransObject(sqlca)
dw_1.SetRowFocusIndicator(Hand!)

Tab_1.TabPage_1.dw_especies.SetTransObject(sqlca)
Tab_1.TabPage_1.dw_especies.InsertRow(0)
Tab_1.TabPage_1.dw_especies.Object.espe_codigo[1] = Integer(istr_Busq.Argum[2])

is_ordena = 'Nº. Planilla:ccre_numero,Planta:plde_codigo,Zona:zona_codigo,Productor:prod_codigo'

TriggerEvent("ue_ordenamiento")

istr_Busq.Argum[30]	= '1'			//Se Utiliza Como Sw en la maed.
ii_fila = 1
Tab_1.TabPage_1.pb_Filtrar.PostEvent(Clicked!)
end event

on w_busc_agrupavariedades.create
int iCurrent
call super::create
end on

on w_busc_agrupavariedades.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[1]	=	String(dw_1.object.espe_codigo[ii_Fila])
istr_busq.argum[2]	=	String(dw_1.object.agva_codigo[ii_fila])
istr_busq.argum[3]	=	dw_1.object.agva_nombre[ii_fila]

CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_agrupavariedades
boolean visible = false
integer x = 2665
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_agrupavariedades
integer y = 760
integer width = 2551
integer height = 900
integer taborder = 60
string dataobject = "dw_mues_agrupavariedades"
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

type pb_salir from w_busqueda`pb_salir within w_busc_agrupavariedades
integer x = 2747
integer y = 1412
end type

event pb_salir::clicked;istr_busq.argum[3]	=	""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_agrupavariedades
integer x = 55
integer y = 92
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
dw_especies dw_especies
st_2 st_2
end type

on tabpage_1.create
this.dw_especies=create dw_especies
this.st_2=create st_2
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_especies
this.Control[iCurrent+2]=this.st_2
end on

on tabpage_1.destroy
call super::destroy
destroy(this.dw_especies)
destroy(this.st_2)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2304
integer y = 184
end type

event pb_filtrar::clicked;
IF dw_1.Retrieve(Integer(istr_Busq.Argum[2])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	tab_1.tabpage_1.dw_Especies.Enabled = False
	tab_1.tabpage_1.dw_especies.Object.espe_codigo.BackGround.Color	=	553648127
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

type dw_especies from datawindow within tabpage_1
integer x = 626
integer y = 192
integer width = 869
integer height = 100
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[2] = data


end event

type st_2 from statictext within tabpage_1
integer x = 302
integer y = 204
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

