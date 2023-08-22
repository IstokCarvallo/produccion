$PBExportHeader$w_busc_ctlcalplanilladestino.srw
forward
global type w_busc_ctlcalplanilladestino from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type cbx_cliente from checkbox within tabpage_1
end type
type dw_cliente from datawindow within tabpage_1
end type
end forward

global type w_busc_ctlcalplanilladestino from w_busqueda
integer x = 78
integer y = 176
integer width = 3035
integer height = 1876
string title = "Búsqueda de Planillas inspección en Destino"
boolean resizable = false
end type
global w_busc_ctlcalplanilladestino w_busc_ctlcalplanilladestino

type variables
DataWindowChild	idwc_cliente

Integer	ii_fila
end variables

event open;Long		ll_fila = 1

x	=	30
y	=	280

istr_busq = Message.PowerObjectParm

tab_1.tabpage_1.dw_cliente.GetChild("clie_codigo", idwc_cliente)

idwc_cliente.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

idwc_cliente.Retrieve(Integer(istr_busq.argum[1]),1)

tab_1.tabpage_1.dw_cliente.SetTransObject(sqlca)
tab_1.tabpage_1.dw_cliente.InsertRow(0)

is_ordena = 'Planilla:cpde_numero,Tipo Transporte:nave_tipotr,Nave:nave_nombre,Mercado:merc_nombre'

TriggerEvent("ue_ordenamiento")
istr_busq.argum[2]	= '999'

istr_busq.argum[30]	= '1'			//Se Utiliza Como Sw en la maed.

tab_1.tabpage_1.pb_filtrar.TriggerEvent(Clicked!)
end event

on w_busc_ctlcalplanilladestino.create
int iCurrent
call super::create
end on

on w_busc_ctlcalplanilladestino.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[1]	=	String(dw_1.object.clie_codigo[dw_1.GetRow()])
istr_busq.argum[2]	=	String(dw_1.object.cpde_numero[dw_1.GetRow()])
istr_busq.argum[3]   =  String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_busq.argum[4]   =  String(dw_1.Object.merc_codigo[dw_1.GetRow()])
istr_busq.argum[5]   =  String(dw_1.Object.dest_codigo[dw_1.GetRow()])

istr_busq.argum[6]   =  dw_1.Object.nave_tipotr[dw_1.GetRow()]
istr_busq.argum[7]   =  String(dw_1.Object.nave_codigo[dw_1.GetRow()])
istr_busq.argum[8]   =  dw_1.Object.cpde_bode01[dw_1.GetRow()]
istr_busq.argum[9]   =  dw_1.Object.cpde_bode02[dw_1.GetRow()]
istr_busq.argum[10]  =  dw_1.Object.cpde_bode03[dw_1.GetRow()]
istr_busq.argum[11]  =  dw_1.Object.cpde_bode04[dw_1.GetRow()]
istr_busq.argum[12]  =  dw_1.Object.cpde_bode05[dw_1.GetRow()]
istr_busq.argum[13]  =  dw_1.Object.cpde_conten[dw_1.GetRow()]
istr_busq.argum[14]  =  String(dw_1.Object.puer_codigo[dw_1.GetRow()])
istr_busq.argum[15]  =  String(dw_1.Object.ccsi_codigo[dw_1.GetRow()])
istr_busq.argum[16]  =  String(dw_1.Object.cpde_fecarr[dw_1.GetRow()])
istr_busq.argum[17]  =  String(dw_1.Object.cpde_fecdes[dw_1.GetRow()])
istr_busq.argum[18]  =  String(dw_1.Object.cpde_fecins[dw_1.GetRow()])
istr_busq.argum[19]  =  String(dw_1.Object.cpde_fecfum[dw_1.GetRow()])
istr_busq.argum[20]  =  String(dw_1.Object.reci_codigo[dw_1.GetRow()])
istr_busq.argum[21]  =  String(dw_1.Object.ccin_codigo[dw_1.GetRow()])

istr_busq.argum[22]  =  String(dw_1.Object.cpde_reclam[dw_1.GetRow()])
istr_busq.argum[23]  =  String(dw_1.Object.prod_codigo[dw_1.GetRow()])
istr_busq.argum[24]  =  String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_busq.argum[25]  =  String(dw_1.Object.cpde_tamlot[dw_1.GetRow()])
istr_busq.argum[26]  =  dw_1.Object.emba_codigo[dw_1.GetRow()]
istr_busq.argum[27]  =  String(dw_1.Object.etiq_codigo[dw_1.GetRow()])
istr_busq.argum[28]  =  dw_1.Object.cpde_calibr[dw_1.GetRow()]




CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_ctlcalplanilladestino
boolean visible = false
integer x = 2665
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_ctlcalplanilladestino
integer y = 760
integer width = 2551
integer height = 900
integer taborder = 60
string dataobject = "dw_busc_mues_planilladestino"
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_fila	=	row

end event

type pb_salir from w_busqueda`pb_salir within w_busc_ctlcalplanilladestino
integer x = 2747
integer y = 1412
end type

event pb_salir::clicked;istr_busq.argum[1]	=	''
istr_busq.argum[2]	=	''

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_ctlcalplanilladestino
integer x = 69
integer y = 96
integer width = 2555
integer height = 628
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2519
integer height = 500
string text = "Filtros                         "
st_1 st_1
cbx_cliente cbx_cliente
dw_cliente dw_cliente
end type

on tabpage_1.create
this.st_1=create st_1
this.cbx_cliente=create cbx_cliente
this.dw_cliente=create dw_cliente
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.cbx_cliente
this.Control[iCurrent+3]=this.dw_cliente
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.cbx_cliente)
destroy(this.dw_cliente)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2309
integer y = 184
end type

event pb_filtrar::clicked;Integer li_cliente

IF cbx_cliente.Checked THEN
	li_cliente =0
ELSE
	li_cliente = dw_cliente.Object.clie_codigo[1]
END IF
	
IF dw_1.Retrieve(li_cliente) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	IF tab_1.tabpage_1.dw_cliente.Enabled THEN
		tab_1.tabpage_1.dw_cliente.SetFocus()
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

type st_1 from statictext within tabpage_1
integer x = 302
integer y = 200
integer width = 274
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 30586022
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type cbx_cliente from checkbox within tabpage_1
integer x = 626
integer y = 92
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 30586022
string text = "Todos"
boolean checked = true
end type

event clicked;String	ls_null

SetNull(ls_null)

IF This.Checked THEN
	dw_cliente.Enabled	=	False
   dw_cliente.SetItem(1,"clie_codigo", Integer(ls_null))
	istr_busq.argum[2]	= '999'
ELSE
	dw_cliente.Enabled	=	True
END IF

end event

type dw_cliente from datawindow within tabpage_1
integer x = 626
integer y = 184
integer width = 1161
integer height = 104
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

