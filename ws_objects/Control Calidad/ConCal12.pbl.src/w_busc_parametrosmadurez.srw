$PBExportHeader$w_busc_parametrosmadurez.srw
forward
global type w_busc_parametrosmadurez from w_busqueda
end type
type st_2 from statictext within tabpage_1
end type
type dw_especies from datawindow within tabpage_1
end type
end forward

global type w_busc_parametrosmadurez from w_busqueda
integer x = 78
integer y = 176
integer width = 3008
integer height = 1832
string title = "Búsqueda Planillas de Parametros de madurez"
boolean resizable = false
end type
global w_busc_parametrosmadurez w_busc_parametrosmadurez

type variables
DataWindowChild	idwc_planta,idwc_prod,idwc_espe

Integer	ii_fila
Integer  ii_SwOrden[]	=	{0, 0, 0}
end variables

event open;Long		ll_fila = 1

x	=	30
y	=	280

istr_busq = Message.PowerObjectParm

tab_1.tabpage_1.dw_especies.GetChild("espe_codigo", idwc_espe)
idwc_espe.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)
dw_1.SetRowFocusIndicator(Hand!)

idwc_espe.Retrieve()
tab_1.tabpage_1.dw_especies.SetTransObject(sqlca)
tab_1.tabpage_1.dw_especies.InsertRow(0)

is_ordena = 'Zona:zona_codigo,Variedad:vari_codigo,Productor:prod_codigo,Predio:prpr_codigo'

TriggerEvent("ue_ordenamiento")
tab_1.tabpage_1.dw_especies.Object.espe_codigo[1] = Integer(istr_busq.argum[1])	


ii_fila = 1

tab_1.tabpage_1.pb_Filtrar.PostEvent(Clicked!)
end event

on w_busc_parametrosmadurez.create
int iCurrent
call super::create
end on

on w_busc_parametrosmadurez.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[1]	=	String(dw_1.Object.espe_codigo[ii_fila])
istr_busq.argum[2]	=	String(dw_1.Object.zona_codigo[ii_fila])
istr_busq.argum[3]	=	String(dw_1.Object.vari_codigo[ii_fila])
istr_busq.argum[4]	=	String(dw_1.Object.prod_codigo[ii_fila])
istr_busq.argum[6]	=	String(dw_1.Object.prpr_codigo[ii_fila])


CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_parametrosmadurez
boolean visible = false
integer x = 2665
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_parametrosmadurez
integer y = 760
integer width = 2551
integer height = 900
integer taborder = 60
string dataobject = "dw_busc_parametrosmadurez"
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

event dw_1::clicked;String	ls_Tecla, ls_Columna, ls_Orden[] = {" A", " D"}, &
			ls_Columnas[]	=	{"zona_codigo", "vari_codigo", "prod_codigo", "prpr_codigo"}, &
			ls_Ordenam		=	"&Zona01&Variedad02&Productor03&Predio04"
Long		ll_Fila, ll_Numero
Integer	li_Posicion

IF Row > 0 THEN
   ii_fila	=	Row
	This.SelectRow(0,False)
	This.SelectRow(Row,True)
	This.SetRow(Row)
//ELSE
	ll_Numero	=	dw_1.Object.zona_codigo[ii_fila]
	ls_Columna	=	dwo.Name
	li_Posicion	=	Pos(ls_Ordenam, "&" + ls_Columna) + 1
	li_Posicion	=	Integer(Mid(ls_Ordenam, li_Posicion + Len(ls_Columna), 2))

	IF li_Posicion > 0 THEN
	 dw_1.SetSort(ls_Columnas[li_Posicion] + ls_Orden[ii_SwOrden[li_Posicion] + 1])
	 dw_1.Sort()
	 ii_SwOrden[li_Posicion]	=	Mod(ii_SwOrden[li_Posicion] + 1, 2)
	 ii_fila		=	dw_1.Find("zona_codigo = " + String(ll_Numero), 1, dw_1.RowCount())
	END IF
END IF
end event

type pb_salir from w_busqueda`pb_salir within w_busc_parametrosmadurez
integer x = 2747
integer y = 1412
end type

event pb_salir::clicked;istr_busq.argum[4]	=	""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_parametrosmadurez
integer x = 69
integer y = 96
integer width = 2555
integer height = 628
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2519
integer height = 500
string text = "Filtros                         "
st_2 st_2
dw_especies dw_especies
end type

on tabpage_1.create
this.st_2=create st_2
this.dw_especies=create dw_especies
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.dw_especies
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_2)
destroy(this.dw_especies)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2309
integer y = 184
end type

event pb_filtrar::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
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
integer y = 228
integer width = 274
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type dw_especies from datawindow within tabpage_1
integer x = 667
integer y = 204
integer width = 869
integer height = 100
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[1] = data
end event

