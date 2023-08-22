$PBExportHeader$w_busc_movtofrutacomenca.srw
$PBExportComments$Ventana de Busqueda  de Movimientos de Fruta Comercial.
forward
global type w_busc_movtofrutacomenca from w_busqueda
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

global type w_busc_movtofrutacomenca from w_busqueda
integer x = 123
integer y = 304
integer width = 2971
string title = "Búsqueda de Movimiento de Fruta Comercial"
end type
global w_busc_movtofrutacomenca w_busc_movtofrutacomenca

type variables
datawindowchild  idwc_especie, idwc_planta


end variables

on w_busc_movtofrutacomenca.create
int iCurrent
call super::create
end on

on w_busc_movtofrutacomenca.destroy
call super::destroy
end on

event ue_asignacion();istr_Busq.Argum[1]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[2]	= String(dw_1.Object.tpmv_codigo[dw_1.GetRow()])
istr_Busq.Argum[3]	= String(dw_1.Object.mfco_numero[dw_1.GetRow()])
istr_Busq.Argum[4]	= String(dw_1.Object.mfco_fecmov[dw_1.GetRow()])
istr_Busq.Argum[5]	= dw_1.Object.clpr_rut[dw_1.GetRow()]
istr_Busq.Argum[6]	= dw_1.Object.clpr_nombre[dw_1.GetRow()]
istr_Busq.Argum[7]	= String(dw_1.Object.tran_codigo[dw_1.GetRow()])
istr_Busq.Argum[8]	= String(dw_1.Object.mfco_guisii[dw_1.GetRow()])
istr_Busq.Argum[9]	= String(dw_1.Object.mfco_estmov[dw_1.GetRow()])
istr_Busq.Argum[10]	= String(dw_1.Object.mfco_tipdoc[dw_1.GetRow()])
istr_Busq.Argum[11]	= String(dw_1.Object.mfco_docrel[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

event resize;call super::resize;Long		maximo

IF tab_1.Width > dw_1.Width THEN
	maximo = tab_1.Width
ELSE
	maximo = dw_1.Width
END IF

This.Width		= maximo + 540


pb_salir.x		= This.WorkSpaceWidth() - 292
pb_salir.y		= This.WorkSpaceHeight() - 264
pb_insertar.x	= This.WorkSpaceWidth() - 292
pb_insertar.y	= pb_salir.y - 304
tab_1.x			= 78
tab_1.y			= 69
dw_1.x			= 78
dw_1.y			= tab_1.Height + 136

end event

event open;call super::open;/*
	Argumentos
		istr_Busq.Argum[1]	:	Planta
		istr_Busq.Argum[2]	:	Tipo de Movimiento
		istr_Busq.Argum[3]	:	Estado del Movimiento
		istr_Busq.Argum[4]	:	                                                                                                               
*/

istr_busq	=	Message.PowerObjectParm
is_ordena 	=	'Número:mfco_numero,Fecha Movto.:mfco_fecmov,Cliente:clpr_rut,' + &
					'Transportista:tran_codigo'

tab_1.tabpage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)

IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_especie.InsertRow(0)
END IF

tab_1.tabpage_1.dw_4.GetChild("tpmv_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)

IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipos de Movimientos")
	idwc_especie.InsertRow(0)
END IF

tab_1.tabpage_1.dw_4.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_4.InsertRow(0)
tab_1.tabpage_1.dw_5.InsertRow(0)

IF istr_Busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_Busq.Argum[1]))
	tab_1.tabpage_1.dw_5.Enabled	=	False
END IF

IF istr_Busq.Argum[2] <> "" THEN
	tab_1.tabpage_1.ddlb_estado.Reset()
	
	IF istr_Busq.Argum[2] = '21' or istr_Busq.Argum[2] = '32' or istr_Busq.Argum[2] = '35' THEN
		tab_1.tabpage_1.ddlb_estado.AddItem('Transitoria')
		tab_1.tabpage_1.ddlb_estado.AddItem('Conf.Packing')
		tab_1.tabpage_1.ddlb_estado.AddItem('Definitiva')
	ELSE
		tab_1.tabpage_1.ddlb_estado.AddItem('Transitoria')
		tab_1.tabpage_1.ddlb_estado.AddItem('Definitiva')
	END IF
	tab_1.tabpage_1.dw_4.SetItem(1, "tpmv_codigo", Integer(istr_Busq.Argum[2]))
	tab_1.tabpage_1.dw_4.Enabled	=	False
END IF

IF istr_Busq.Argum[3] <> "" THEN
	IF istr_Busq.Argum[2] <> '21' AND istr_Busq.Argum[3] = '3' THEN
		tab_1.tabpage_1.ddlb_estado.SelectItem(2)
	ELSE
		tab_1.tabpage_1.ddlb_estado.SelectItem(Integer(istr_Busq.Argum[3]))
	END IF
	tab_1.tabpage_1.ddlb_estado.Enabled	=	False
ELSE
	tab_1.tabpage_1.ddlb_estado.SelectItem(1)
	tab_1.tabpage_1.ddlb_estado.Enabled	=	True
	istr_busq.Argum[3]	=	'1'
END IF

IF istr_Busq.Argum[4] <> "" THEN
   tab_1.tabpage_1.em_fechamovto.text	=	Mid(istr_Busq.Argum[4], 1, 10)
ELSE
	istr_busq.Argum[4]	=	String(Today(),'DD/MM/YYYY')
	tab_1.tabpage_1.em_fechamovto.text	=	Mid(istr_Busq.Argum[4], 1, 10)
END IF

istr_Mant.Argumento[1]	=	istr_Busq.Argum[1]
istr_Mant.Argumento[2]	=	istr_Busq.Argum[2]
istr_Mant.Argumento[3]	=	istr_Busq.Argum[3]
istr_Mant.Argumento[4]	=	istr_Busq.Argum[4]
istr_Busq.Argum[1]		=	""
istr_Busq.Argum[2]		=	""
istr_Busq.Argum[3]		=	""
istr_Busq.Argum[4]		=	""
istr_Busq.Argum[13]		=	""

tab_1.tabpage_1.pb_filtrar.SetFocus()
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_movtofrutacomenca
boolean visible = false
integer x = 2670
integer y = 988
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_movtofrutacomenca
integer x = 101
integer y = 656
integer width = 2459
integer height = 948
integer taborder = 60
string dataobject = "dw_mues_movtofrutacomenca"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_movtofrutacomenca
integer x = 2665
integer y = 1276
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_movtofrutacomenca
integer x = 101
integer y = 80
integer width = 2459
integer height = 544
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2423
integer height = 416
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
integer y = 248
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;DateTime	ld_FechaMovto

IF NOT IsDate(em_fechamovto.Text) THEN
	MessageBox("Atención","Falta ingresar la Fecha de Movimiento de Inicio.")
	RETURN
END IF

ld_FechaMovto	=	DateTime(Date(em_fechamovto.Text),Time('00:00'))

IF dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
					  Integer(istr_Mant.Argumento[2]), &
					  Integer(istr_Mant.Argumento[3]), ld_FechaMovto) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2423
integer height = 416
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1760
integer y = 272
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
integer height = 364
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
integer height = 360
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2423
integer height = 416
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
integer x = 663
integer y = 168
long backcolor = 12632256
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
is_busca							= "clpr_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
integer y = 176
string text = "Cliente"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 663
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
is_busca							= "mfco_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer y = 56
integer width = 453
string text = "Número Movto."
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2226
integer y = 248
end type

type st_1 from statictext within tabpage_1
integer x = 64
integer y = 52
integer width = 288
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
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
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
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
integer width = 366
integer height = 96
integer taborder = 35
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Fecha Inicio Movto."
boolean focusrectangle = false
end type

type st_5 from statictext within tabpage_1
integer x = 64
integer y = 300
integer width = 402
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
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
long textcolor = 33554432
string text = "none"
boolean sorted = false
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF istr_Mant.Argumento[2] <> '21' AND index = 2 THEN
//	istr_Mant.Argumento[3] = '3'
	istr_Mant.Argumento[3] = String(index)
ELSE
	istr_Mant.Argumento[3] = String(index)
END IF
end event

type st_3 from st_argum2 within tabpage_3
integer y = 296
integer width = 389
boolean bringtotop = true
string text = "Transportista"
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
es_numero						= True
is_busca							= "tran_codigo"
end event

