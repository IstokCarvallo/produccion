$PBExportHeader$w_busc_lotefrutagranel.srw
forward
global type w_busc_lotefrutagranel from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_plantas from datawindow within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_especies from datawindow within tabpage_1
end type
type st_3 from statictext within tabpage_1
end type
type uo_selcliente from uo_seleccion_clientesprod within tabpage_1
end type
end forward

global type w_busc_lotefrutagranel from w_busqueda
integer x = 78
integer y = 176
integer width = 3131
integer height = 1832
string title = "Búsqueda de Lotes"
boolean resizable = false
end type
global w_busc_lotefrutagranel w_busc_lotefrutagranel

type variables
DataWindowChild	idwc_planta,idwc_prod,idwc_espe

Integer	ii_fila
Integer  ii_SwOrden[]	=	{0, 0, 0}
end variables

event open;Long		ll_fila = 1

x	=	30
y	=	280

istr_busq = Message.PowerObjectParm	

Tab_1.TabPage_1.dw_plantas.GetChild("plde_codigo", idwc_planta)
Tab_1.TabPage_1.dw_especies.GetChild("espe_codigo", idwc_espe)

idwc_planta.SetTransObject(sqlca)
idwc_espe.SetTransObject(sqlca)

IF istr_busq.argum[5] =	'RECEPCION' THEN
	dw_1.DataObject	=	"dw_mues_lotesfrutagranel"
	Tab_1.TabPage_1.uo_SelCliente.Visible = True
	Tab_1.TabPage_1.st_3.Visible = True
	
	Tab_1.TabPage_1.uo_SelCliente.Seleccion(False, False)
	Tab_1.TabPage_1.uo_SelCliente.Codigo = Long(istr_Busq.Argum[8])
	Tab_1.TabPage_1.uo_SelCliente.dw_Seleccion.Object.Codigo[1] = Long(istr_Busq.Argum[8])
ElseIf istr_busq.argum[5] =	'Comercial' THEN
	dw_1.DataObject	=	"dw_mues_lotescomercial"
ELSE
	dw_1.DataObject	=	"dw_mues_lotesdescartafruta"
	Tab_1.TabPage_1.uo_SelCliente.Visible = True
	Tab_1.TabPage_1.st_3.Visible = True
	
	Tab_1.TabPage_1.uo_SelCliente.Seleccion(False, False)
	Tab_1.TabPage_1.uo_SelCliente.Codigo = Long(istr_Busq.Argum[8])
	Tab_1.TabPage_1.uo_SelCliente.dw_Seleccion.Object.Codigo[1] = Long(istr_Busq.Argum[8])
END IF

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

//idwc_planta.Retrieve(1,Integer(istr_busq.argum[4]))
idwc_planta.Retrieve(1,-1)
idwc_espe.Retrieve()

Tab_1.TabPage_1.dw_plantas.SetTransObject(sqlca)
Tab_1.TabPage_1.dw_plantas.InsertRow(0)

Tab_1.TabPage_1.dw_especies.SetTransObject(sqlca)
Tab_1.TabPage_1.dw_especies.InsertRow(0)
Tab_1.TabPage_1.dw_especies.Object.espe_codigo.protect	=	1
Tab_1.TabPage_1.dw_especies.Object.espe_codigo.BackGround.Color	=	553648127


is_ordena = 'Nº. Lote:lote_codigo,Variedad:vari_codigo,Productor:prod_codigo'

TriggerEvent("ue_ordenamiento")
Tab_1.TabPage_1.dw_plantas.Object.plde_codigo[1]  = Integer(istr_busq.argum[1])
Tab_1.TabPage_1.dw_especies.Object.espe_codigo[1] = Integer(istr_busq.argum[2])

ii_fila = 1

Tab_1.TabPage_1.pb_Filtrar.PostEvent(Clicked!)
end event

on w_busc_lotefrutagranel.create
int iCurrent
call super::create
end on

on w_busc_lotefrutagranel.destroy
call super::destroy
end on

event ue_asignacion;IF istr_busq.argum[5] =	'RECEPCION' THEN
	istr_busq.argum[2]	=  String(dw_1.Object.lote_codigo[ii_fila])
	istr_busq.argum[3]	=  String(dw_1.Object.lote_pltcod[ii_fila])
	istr_busq.argum[5]	=	String(dw_1.Object.prod_codigo[ii_fila])
	istr_busq.argum[6]	=  String(dw_1.Object.vari_codigo[ii_fila])
	istr_busq.argum[8]	=	String(dw_1.Object.prbr_codpre[ii_fila])
	istr_busq.argum[9]	=	String(dw_1.Object.prcc_codigo[ii_fila])
	istr_busq.argum[10]	=	String(dw_1.Object.lote_totbul[ii_fila])
	istr_busq.argum[11]	=	String(dw_1.Object.fgcc_fecrec[ii_fila])
	istr_busq.argum[12]	=	String(dw_1.Object.lote_guisii[ii_fila])
	istr_busq.argum[13]	=	String(dw_1.Object.fgcc_feccos[ii_fila])
	istr_busq.argum[14]	=	String(dw_1.Object.lote_espcod[ii_fila])
	istr_busq.argum[21]	=  String(dw_1.Object.lote_calibr[ii_fila])
	istr_busq.argum[22]	=  String(dw_1.Object.lote_conhid[ii_fila])
	istr_busq.argum[23]	=  String(dw_1.Object.clie_codigo[ii_fila])
	istr_busq.argum[24]	=  String(dw_1.Object.lote_totnet[ii_fila])
ElseIf istr_busq.argum[5] =	'Comercial' THEN
	istr_busq.argum[2]	=  String(dw_1.Object.lofc_lotefc[ii_fila])
	istr_busq.argum[3]	=  String(dw_1.Object.lofc_pltcod[ii_fila])
	istr_busq.argum[5]	=	String(dw_1.Object.prod_codigo[ii_fila])
	istr_busq.argum[6]	=  String(dw_1.Object.vari_codigo[ii_fila])
	istr_busq.argum[11]	=	String(dw_1.Object.lfcd_fecham[ii_fila])
	istr_busq.argum[14]	=	String(dw_1.Object.lofc_espcod[ii_fila])	
	istr_busq.argum[15]	=	String(dw_1.Object.cate_codigo[ii_fila])	
	istr_busq.argum[16]	=	String(dw_1.Object.zona_codigo[ii_fila])	
	istr_busq.argum[24]	=  String(dw_1.Object.lote_totnet[ii_fila])	
ELSE
	istr_busq.argum[2]	=  String(dw_1.Object.lote_codigo[ii_fila])
	istr_busq.argum[3]	=  String(dw_1.Object.lote_pltcod[ii_fila])
	istr_busq.argum[5]	=	String(dw_1.Object.prod_codigo[ii_fila])
	istr_busq.argum[6]	=  String(dw_1.Object.vari_codigo[ii_fila])
	istr_busq.argum[8]	=	String(dw_1.Object.prbr_codpre[ii_fila])
	istr_busq.argum[9]	=	String(dw_1.Object.prcc_codigo[ii_fila])
	istr_busq.argum[10]	=	String(dw_1.Object.lote_totbul[ii_fila])
	istr_busq.argum[11]	=	String(dw_1.Object.fgcc_fecrec[ii_fila])
	istr_busq.argum[12]	=	String(dw_1.Object.lote_guisii[ii_fila])
	istr_busq.argum[13]	=	String(dw_1.Object.fgcc_feccos[ii_fila])
	istr_busq.argum[14]	=	String(dw_1.Object.lote_espcod[ii_fila])
	istr_busq.argum[16]	=	String(dw_1.Object.zona_codigo[ii_fila])	
	istr_busq.argum[21]	= ''
	istr_busq.argum[24]	=  String(dw_1.Object.lote_totnet[ii_fila])
END IF


CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_lotefrutagranel
boolean visible = false
integer x = 2747
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_lotefrutagranel
integer y = 760
integer width = 2551
integer height = 900
integer taborder = 60
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row
istr_busq.argum[30]	= '0'
end event

type pb_salir from w_busqueda`pb_salir within w_busc_lotefrutagranel
integer x = 2747
integer y = 1412
end type

event pb_salir::clicked;istr_busq.argum[3]	=	""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_lotefrutagranel
integer x = 69
integer y = 100
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
st_3 st_3
uo_selcliente uo_selcliente
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_plantas=create dw_plantas
this.st_2=create st_2
this.dw_especies=create dw_especies
this.st_3=create st_3
this.uo_selcliente=create uo_selcliente
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_plantas
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_especies
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.uo_selcliente
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_plantas)
destroy(this.st_2)
destroy(this.dw_especies)
destroy(this.st_3)
destroy(this.uo_selcliente)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2304
integer y = 188
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_filtrar::clicked;
IF istr_busq.argum[5] =	'RECEPCION' THEN
	IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]),&
		Integer(istr_busq.argum[6]),Long(istr_busq.argum[7]),Integer(istr_busq.argum[20]),Long(istr_busq.argum[8])) > 0 THEN
		dw_1.SetFocus()
		dw_1.SelectRow(1,True)
	ELSE
		MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
		tab_1.tabpage_1.dw_plantas.Enabled = TRUE
		tab_1.tabpage_1.dw_plantas.SetFocus()
	END IF
ElseIf istr_busq.argum[5] =	'Comercial' THEN
	IF dw_1.Retrieve(Integer(istr_busq.argum[2]), Integer(istr_busq.argum[6]), Long(istr_busq.argum[7])) > 0 THEN
		dw_1.SetFocus()
		dw_1.SelectRow(1,True)
	ELSE
		MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
		tab_1.tabpage_1.dw_plantas.Enabled = TRUE
		tab_1.tabpage_1.dw_plantas.SetFocus()
	END IF
ELSE
	IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]),&
		Integer(istr_busq.argum[6]),Long(istr_busq.argum[7]), Long(istr_busq.argum[8])) > 0 THEN
		dw_1.SetFocus()
		dw_1.SelectRow(1,True)
	ELSE
		MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
		tab_1.tabpage_1.dw_plantas.Enabled = TRUE
		tab_1.tabpage_1.dw_plantas.SetFocus()
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
integer height = 88
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

type st_3 from statictext within tabpage_1
boolean visible = false
integer x = 297
integer y = 392
integer width = 274
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

type uo_selcliente from uo_seleccion_clientesprod within tabpage_1
boolean visible = false
integer x = 626
integer y = 380
integer height = 80
integer taborder = 30
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;istr_Busq	.Argum[8] = String(This.Codigo)
end event

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

