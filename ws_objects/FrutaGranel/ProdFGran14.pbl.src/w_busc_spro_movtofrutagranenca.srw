$PBExportHeader$w_busc_spro_movtofrutagranenca.srw
forward
global type w_busc_spro_movtofrutagranenca from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_4 from datawindow within tabpage_1
end type
type dw_5 from datawindow within tabpage_1
end type
type em_fechamovto from editmask within tabpage_1
end type
type st_4 from statictext within tabpage_1
end type
type st_5 from statictext within tabpage_1
end type
type ddlb_estado from dropdownlistbox within tabpage_1
end type
type st_3 from st_argum2 within tabpage_3
end type
type sle_argumento3 from sle_argumento2 within tabpage_3
end type
end forward

global type w_busc_spro_movtofrutagranenca from w_busqueda
integer x = 123
integer y = 304
integer width = 3369
integer height = 2044
string title = "Búsqueda de Movimiento de Fruta"
end type
global w_busc_spro_movtofrutagranenca w_busc_spro_movtofrutagranenca

type variables
datawindowchild  idwc_especie, idwc_planta, idwc_especiedet, idwc_productor


end variables

on w_busc_spro_movtofrutagranenca.create
int iCurrent
call super::create
end on

on w_busc_spro_movtofrutagranenca.destroy
call super::destroy
end on

event ue_asignacion();istr_Busq.Argum[1]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[2]	= String(dw_1.Object.tpmv_codigo[dw_1.GetRow()])
istr_Busq.Argum[3]	= String(dw_1.Object.mfge_numero[dw_1.GetRow()])
istr_Busq.Argum[4]	= String(dw_1.Object.mfge_fecmov[dw_1.GetRow()])
istr_Busq.Argum[5]	= String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_Busq.Argum[6]	= String(dw_1.Object.prod_codigo[dw_1.GetRow()])
istr_Busq.Argum[7]	= String(dw_1.Object.tran_codigo[dw_1.GetRow()])
istr_Busq.Argum[8]	= String(dw_1.Object.mfge_guisii[dw_1.GetRow()])
istr_Busq.Argum[9]	= String(dw_1.Object.mfge_estmov[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

event open;call super::open;/*
	Argumentos
		istr_Busq.Argum[1]	:	Planta
		istr_Busq.Argum[2]	:	Tipo de Movimiento
		istr_Busq.Argum[3]	:	Estado del Movimiento
		istr_Busq.Argum[4]	:	  
		istr_Busq.Argum[10]  :  Cliente
*/    

istr_busq	=	Message.PowerObjectParm
is_ordena 	=	'Número:mfge_numero,Fecha Movto.:mfge_fecmov,Especie:espe_codigo'
Tab_1.TabPage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)


IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_especie.InsertRow(0)
END IF

Tab_1.TabPage_1.dw_4.GetChild("tpmv_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)


IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipos de Movimientos")
	idwc_especie.InsertRow(0)
END IF

Tab_1.TabPage_1.dw_4.SetTransObject(SQLCA)
Tab_1.TabPage_1.dw_5.SetTransObject(SQLCA)

Tab_1.TabPage_1.dw_4.InsertRow(0)
Tab_1.TabPage_1.dw_5.InsertRow(0)

IF istr_Busq.Argum[1] <> "" THEN
	Tab_1.TabPage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_Busq.Argum[1]))
	Tab_1.TabPage_1.dw_5.Enabled	=	False
END IF

IF istr_Busq.Argum[2] <> "" THEN
	Tab_1.TabPage_1.ddlb_estado.Reset()
	
	IF istr_Busq.Argum[2] = '21' OR istr_Busq.Argum[2] = '22' THEN
		Tab_1.TabPage_1.ddlb_estado.AddItem('Transitoria')
		Tab_1.TabPage_1.ddlb_estado.AddItem('Conf.Packing')
		Tab_1.TabPage_1.ddlb_estado.AddItem('Definitiva')
	ELSE
		Tab_1.TabPage_1.ddlb_estado.AddItem('Transitoria')
		Tab_1.TabPage_1.ddlb_estado.AddItem('Definitiva')
	END IF
	Tab_1.TabPage_1.dw_4.SetItem(1, "tpmv_codigo", Integer(istr_Busq.Argum[2]))
	Tab_1.TabPage_1.dw_4.Enabled	=	False
END IF

IF istr_Busq.Argum[3] <> "" THEN
	IF istr_Busq.Argum[2] <> '21' AND istr_Busq.Argum[2] <> '22' AND istr_Busq.Argum[3] = '3' THEN
		Tab_1.TabPage_1.ddlb_estado.SelectItem(2)
	ELSE
		Tab_1.TabPage_1.ddlb_estado.SelectItem(Integer(istr_Busq.Argum[3]))
	END IF
	Tab_1.TabPage_1.ddlb_estado.Enabled	=	False
ELSE
	Tab_1.TabPage_1.ddlb_estado.SelectItem(1)
	Tab_1.TabPage_1.ddlb_estado.Enabled	=	True
	istr_busq.Argum[3]	=	'1'
END IF

IF istr_Busq.Argum[4] <> "" THEN
   Tab_1.TabPage_1.em_FechaMovto.text	=	Mid(istr_Busq.Argum[4], 1, 10)
ELSE
	istr_busq.Argum[4]	=	String(Today(),'DD/MM/YYYY')
	Tab_1.TabPage_1.em_FechaMovto.text	=	String(Today(),'DD/MM/YYYY')
END IF

IF UpperBound(istr_busq.argum) = 5 THEN
	IF istr_Busq.Argum[5] = "1" Then
		dw_1.Object.t_2.Visible = False
		dw_1.Object.prod_codigo.Visible = False
		dw_1.Object.t_3.x = 1367
		dw_1.Object.tran_codigo.x = 1367
	END IF
END IF

istr_Mant.Argumento[1]	=	istr_Busq.Argum[1]
istr_Mant.Argumento[2]	=	istr_Busq.Argum[2]
istr_Mant.Argumento[3]	=	istr_Busq.Argum[3]
istr_Mant.Argumento[4]	=	istr_Busq.Argum[4]
istr_Mant.Argumento[10]	=	istr_Busq.Argum[10]
istr_Busq.Argum[1]		=	""
istr_Busq.Argum[2]		=	""
istr_Busq.Argum[3]		=	""
istr_Busq.Argum[4]		=	""
istr_Busq.Argum[13]		=	""

Tab_1.TabPage_1.pb_filtrar.SetFocus()
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_movtofrutagranenca
boolean visible = false
integer x = 2981
integer y = 936
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_movtofrutagranenca
integer x = 41
integer y = 708
integer width = 2816
integer height = 948
integer taborder = 60
string dataobject = "dw_mues_spro_movtofrutagranenca"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_spro_movtofrutagranenca
integer x = 3003
integer y = 1276
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_spro_movtofrutagranenca
integer x = 59
integer y = 80
integer width = 2615
integer height = 580
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
integer width = 2578
integer height = 452
string text = "Filtros              "
st_1 st_1
st_2 st_2
dw_4 dw_4
dw_5 dw_5
em_fechamovto em_fechamovto
st_4 st_4
st_5 st_5
ddlb_estado ddlb_estado
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.dw_4=create dw_4
this.dw_5=create dw_5
this.em_fechamovto=create em_fechamovto
this.st_4=create st_4
this.st_5=create st_5
this.ddlb_estado=create ddlb_estado
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.em_fechamovto
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.ddlb_estado
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.em_fechamovto)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.ddlb_estado)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2217
integer y = 244
integer width = 233
integer height = 196
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;Date	ld_FechaMovto

dw_1.GetChild("espe_codigo", idwc_especiedet)
idwc_especiedet.SetTransObject(SqlCa)
IF idwc_especiedet.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención", "Falta Registrar Especies")
	idwc_especiedet.InsertRow(0)
END IF

dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(-1) 
idwc_productor.InsertRow(0)

IF NOT IsDate(em_fechamovto.Text) THEN
	MessageBox("Atención","Falta ingresar la Fecha de Movimiento de Inicio.")
	RETURN
END IF

ld_FechaMovto	=	Date(em_fechamovto.Text)

IF dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
						Integer(istr_Mant.Argumento[2]), &
						Integer(istr_Mant.Argumento[3]), ld_FechaMovto, &
						Integer(istr_Mant.Argumento[10])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2578
integer height = 452
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2217
integer y = 248
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
integer height = 376
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
integer height = 376
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2578
integer height = 452
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
integer y = 168
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
integer y = 176
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 645
integer y = 48
integer width = 306
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= True
is_busca							= "lote_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer y = 56
integer width = 389
string text = "Número Lote"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2217
integer y = 248
end type

type st_1 from statictext within tabpage_1
integer x = 64
integer y = 48
integer width = 288
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 64
integer y = 168
integer width = 352
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Tipo Movto."
boolean focusrectangle = false
end type

type dw_4 from datawindow within tabpage_1
integer x = 489
integer y = 160
integer width = 873
integer height = 96
integer taborder = 31
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_spro_tipomovtofruta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "tpmv_codigo"
		istr_Mant.Argumento[2]	=	data
END CHOOSE
end event

type dw_5 from datawindow within tabpage_1
integer x = 489
integer y = 40
integer width = 873
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "plde_codigo"
		istr_Mant.Argumento[1]	=	data
END CHOOSE
end event

type em_fechamovto from editmask within tabpage_1
integer x = 2007
integer y = 40
integer width = 421
integer height = 96
integer taborder = 35
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;IF This.Text = "" THEN RETURN

istr_Mant.Argumento[4]	=	This.Text
end event

type st_4 from statictext within tabpage_1
integer x = 1422
integer y = 48
integer width = 581
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Fecha Inicio Movto."
boolean focusrectangle = false
end type

type st_5 from statictext within tabpage_1
integer x = 64
integer y = 280
integer width = 402
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Estado"
boolean focusrectangle = false
end type

type ddlb_estado from dropdownlistbox within tabpage_1
integer x = 489
integer y = 288
integer width = 873
integer height = 316
integer taborder = 21
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
string text = "none"
boolean sorted = false
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF istr_Mant.Argumento[2] <> '21' AND index = 2 THEN
	istr_Mant.Argumento[3] = '3'
ELSE
	istr_Mant.Argumento[3] = String(index)
END IF
end event

type st_3 from st_argum2 within tabpage_3
integer y = 296
boolean bringtotop = true
string text = "Productor"
end type

type sle_argumento3 from sle_argumento2 within tabpage_3
integer y = 288
integer taborder = 25
boolean bringtotop = true
end type

event getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= False
is_busca							= "prod_nombre"
end event

