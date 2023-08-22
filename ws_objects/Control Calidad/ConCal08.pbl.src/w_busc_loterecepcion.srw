$PBExportHeader$w_busc_loterecepcion.srw
forward
global type w_busc_loterecepcion from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_plantas from datawindow within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_especies from datawindow within tabpage_1
end type
end forward

global type w_busc_loterecepcion from w_busqueda
integer x = 78
integer y = 176
integer width = 3008
integer height = 1832
string title = "Búsqueda de Planillas de Verificación"
boolean resizable = false
end type
global w_busc_loterecepcion w_busc_loterecepcion

type variables
DataWindowChild	idwc_planta,idwc_prod,idwc_espe

Integer	ii_fila
Integer  ii_SwOrden[]	=	{0, 0, 0}
end variables

event open;Long		ll_fila = 1

x	=	30 
y	=	280

istr_busq = Message.PowerObjectParm

tab_1.tabpage_1.dw_plantas.GetChild("plde_codigo", idwc_planta)
tab_1.tabpage_1.dw_especies.GetChild("espe_codigo", idwc_espe)

idwc_planta.SetTransObject(sqlca)
idwc_espe.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

//idwc_planta.Retrieve(1,Integer(istr_busq.argum[4]))
idwc_planta.Retrieve(1,0)
idwc_espe.Retrieve()

tab_1.tabpage_1.dw_plantas.SetTransObject(sqlca)
tab_1.tabpage_1.dw_plantas.InsertRow(0)

tab_1.tabpage_1.dw_especies.SetTransObject(sqlca)
tab_1.tabpage_1.dw_especies.InsertRow(0)

tab_1.tabpage_1.dw_especies.Object.espe_codigo.protect	=	1
tab_1.tabpage_1.dw_especies.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)

is_ordena = 'Nº. Lote:lote_codigo,Variedad:vari_codigo,Productor:prod_codigo'

TriggerEvent("ue_ordenamiento")
tab_1.tabpage_1.dw_plantas.Object.plde_codigo[1]  = Integer(istr_busq.argum[1])
tab_1.tabpage_1.dw_especies.Object.espe_codigo[1] = Integer(istr_busq.argum[2])	

ii_fila = 1

tab_1.tabpage_1.pb_Filtrar.PostEvent(Clicked!)

end event

on w_busc_loterecepcion.create
int iCurrent
call super::create
end on

on w_busc_loterecepcion.destroy
call super::destroy
end on

event ue_asignacion;Integer li_fila, li_Arg = 5
//istr_busq.argum[2] = String(dw_1.object.lote_codigo[ii_fila])
//istr_busq.argum[3] = String(dw_1.object.ccpv_nlote2[ii_fila])
//istr_busq.argum[4] = String(dw_1.object.ccpv_nlote3[ii_fila])
//istr_busq.argum[5] = String(dw_1.object.ccpv_nlote4[ii_fila])
//istr_busq.argum[6] = String(dw_1.object.ccpv_nlote5[ii_fila])

FOR li_fila = 1 TO dw_1.RowCount()
	
	IF dw_1.IsSelected(li_fila) THEN
		li_Arg ++
		istr_busq.argum[li_Arg] = String(dw_1.object.lote_codigo[li_fila])
	END IF
NEXT
IF li_Arg > 5 THEN RETURN
	
	
CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_loterecepcion
boolean visible = false
integer x = 2665
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_loterecepcion
integer x = 64
integer y = 748
integer width = 2560
integer height = 900
integer taborder = 60
string dataobject = "dw_mues_lotesfrugranel"
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

event dw_1::clicked;String	ls_Tecla

IF Row > 0 THEN
//	il_fila = Row
//	This.SelectRow(0,False)
//	This.SetRow(il_fila)
//	This.SelectRow(il_fila,True)
	IF KeyDown(KeyShift!) THEN
		ls_tecla	=	"Shift"
	ELSEIF KeyDown(KeyControl!) THEN
		ls_tecla	=	"Control"
	END IF
	F_Selecciona(This, ls_Tecla, Row)
END IF

RETURN 0
end event

type pb_salir from w_busqueda`pb_salir within w_busc_loterecepcion
integer x = 2729
integer y = 1412
end type

event pb_salir::clicked;istr_busq.argum[4]	=	""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_loterecepcion
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
st_2 st_2
dw_especies dw_especies
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_plantas=create dw_plantas
this.st_2=create st_2
this.dw_especies=create dw_especies
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_plantas
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_especies
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_plantas)
destroy(this.st_2)
destroy(this.dw_especies)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2309
integer y = 184
end type

event pb_filtrar::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]),&
		           Integer(istr_busq.argum[4]),Long(istr_busq.argum[5])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	IF tab_1.tabpage_1.dw_plantas.Enabled THEN
		tab_1.tabpage_1.dw_plantas.SetFocus()
	ELSE
		tab_1.tabpage_1.dw_plantas.Enabled = TRUE
	END IF
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

type st_1 from statictext within tabpage_1
integer x = 302
integer y = 148
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
integer y = 128
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

type st_2 from statictext within tabpage_1
integer x = 302
integer y = 280
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

type dw_especies from datawindow within tabpage_1
integer x = 626
integer y = 256
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

event itemchanged;istr_busq.argum[2] = data
end event

