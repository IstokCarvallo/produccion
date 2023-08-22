$PBExportHeader$w_busc_verificacionlotes.srw
forward
global type w_busc_verificacionlotes from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_plantas from datawindow within tabpage_1
end type
end forward

global type w_busc_verificacionlotes from w_busqueda
integer x = 78
integer y = 176
integer width = 3118
integer height = 1832
string title = "Búsqueda de Lotes de Verificación"
boolean resizable = false
end type
global w_busc_verificacionlotes w_busc_verificacionlotes

type variables
DataWindowChild	idwc_planta,idwc_variedades, idwc_Productor

Integer	ii_fila
Integer  ii_SwOrden[]	=	{0, 0, 0}
end variables

event open;call super::open;Long		ll_fila = 1

x	=	30 
y	=	280

istr_busq = Message.PowerObjectParm

Tab_1.TabPage_1.dw_plantas.GetChild("plde_codigo", idwc_planta)

If IsNull(istr_busq.argum[7]) Then	istr_busq.argum[7] = '*'
If IsNull(istr_busq.argum[9]) Then istr_busq.argum[9] = '1900/01/01'
If IsNull(istr_busq.argum[12]) Then istr_busq.argum[12] = '*'
If IsNull(istr_busq.argum[14]) Then istr_busq.argum[14] = '-1'

idwc_planta.SetTransObject(sqlca)
dw_1.SetTransObject(sqlca)
dw_1.SetRowFocusIndicator(Hand!)

//idwc_planta.Retrieve(1,Integer(istr_busq.argum[4]))
idwc_planta.Retrieve(1,-1)

Tab_1.TabPage_1.dw_plantas.SetTransObject(sqlca)
Tab_1.TabPage_1.dw_plantas.InsertRow(0)

is_ordena = 'Nº. Lote:lote_codigo,Variedad:vari_codigo,Productor:prod_codigo'

TriggerEvent("ue_ordenamiento")
Tab_1.TabPage_1.dw_plantas.Object.plde_codigo[1]  = Integer(istr_busq.argum[1])

dw_1.GetChild('prod_codigo', idwc_Productor)
idwc_Productor.SetTransObject(sqlca)
idwc_Productor.Retrieve(-1)

dw_1.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(Integer(istr_busq.argum[13]))

ii_fila = 1

Tab_1.TabPage_1.pb_Filtrar.PostEvent(Clicked!)
end event

on w_busc_verificacionlotes.create
int iCurrent
call super::create
end on

on w_busc_verificacionlotes.destroy
call super::destroy
end on

event ue_asignacion;
istr_busq.argum[1]	=	String(dw_1.Object.plde_codigo[ii_fila])
istr_busq.argum[2]	=	String(dw_1.Object.prod_codigo[ii_fila])
istr_busq.argum[3]	=	String(dw_1.Object.cclo_numero[ii_fila])
istr_busq.argum[6]	=	String(dw_1.Object.vari_codigo[ii_fila])
istr_busq.argum[7]	=	String(dw_1.Object.emba_codigo[ii_fila])
istr_busq.argum[8]	=	String(dw_1.Object.vaca_calibr[ii_fila])
istr_busq.argum[9]	=	String(dw_1.Object.cclo_fecemb[ii_fila])
istr_busq.argum[10]	=	String(dw_1.Object.cclo_tamlot[ii_fila])
istr_busq.argum[11]	=	String(dw_1.Object.plde_codpak[ii_fila])

CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_verificacionlotes
boolean visible = false
integer x = 2665
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_verificacionlotes
integer y = 760
integer width = 2551
integer height = 900
integer taborder = 60
string dataobject = "dw_buscallotes_especie"
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

event dw_1::clicked;String	ls_Tecla, ls_Columna, ls_Orden[] = {" A", " D"}, &
			ls_Columnas[]	=	{"ccpv_numero", "plde_codigo", "zona_codigo", "prod_codigo"}, &
			ls_Ordenam		=	"&planilla01&Planta02&Zona03&Productor04"
Long		ll_Fila, ll_Numero
Integer	li_Posicion

IF Row > 0 THEN
   ii_fila	=	Row
	This.SelectRow(0,False)
	This.SelectRow(Row,True)
	This.SetRow(Row)
ELSE
	//ll_Numero	=	dw_1.Object.ccpv_numero[ii_fila]
	ls_Columna	=	dwo.Name
	li_Posicion	=	Pos(ls_Ordenam, "&" + ls_Columna) + 1
	li_Posicion	=	Integer(Mid(ls_Ordenam, li_Posicion + Len(ls_Columna), 2))

	IF li_Posicion > 0 THEN
	 dw_1.SetSort(ls_Columnas[li_Posicion] + ls_Orden[ii_SwOrden[li_Posicion] + 1])
	 dw_1.Sort()
	 ii_SwOrden[li_Posicion]	=	Mod(ii_SwOrden[li_Posicion] + 1, 2)
	 ii_fila		=	dw_1.Find("ccpv_numero = " + String(ll_Numero), 1, dw_1.RowCount())
	END IF
END IF

end event

type pb_salir from w_busqueda`pb_salir within w_busc_verificacionlotes
integer x = 2747
integer y = 1412
end type

event pb_salir::clicked;istr_busq.argum[4]	=	""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_verificacionlotes
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
st_1 st_1
dw_plantas dw_plantas
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_plantas=create dw_plantas
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_plantas
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_plantas)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2309
integer y = 184
end type

event pb_filtrar::clicked;If dw_1.Retrieve(Integer(istr_busq.argum[5]),Integer(istr_busq.argum[1]),&
	      			  Long(istr_busq.argum[2]),istr_busq.argum[7],Date(istr_busq.argum[9]),&
					  Integer(istr_busq.argum[6]),istr_busq.argum[12], Integer(istr_busq.argum[14])) > 0 Then
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
Else
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	If Tab_1.TabPage_1.dw_plantas.Enabled Then
		Tab_1.TabPage_1.dw_plantas.SetFocus()
	Else
		Tab_1.TabPage_1.dw_plantas.Enabled = True
	End If
End If
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

type st_1 from statictext within tabpage_1
integer x = 302
integer y = 212
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
boolean enabled = false
string text = "Planta "
boolean focusrectangle = false
end type

type dw_plantas from datawindow within tabpage_1
integer x = 626
integer y = 200
integer width = 960
integer height = 96
integer taborder = 11
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_plantadesp_tipo_zona"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[1] = data
end event

