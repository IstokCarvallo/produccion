$PBExportHeader$w_busc_spro_movtofrutagranenca_desverd.srw
$PBExportComments$Busqueda de Movimientos de Recepción de Huerto con Filtro por Especie.
forward
global type w_busc_spro_movtofrutagranenca_desverd from w_busqueda
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
type dw_6 from datawindow within tabpage_1
end type
type st_6 from statictext within tabpage_1
end type
type cbx_todas from checkbox within tabpage_1
end type
type ddlb_estado from dropdownlistbox within tabpage_1
end type
type st_3 from st_argum2 within tabpage_3
end type
type sle_argumento3 from sle_argumento2 within tabpage_3
end type
end forward

global type w_busc_spro_movtofrutagranenca_desverd from w_busqueda
integer x = 123
integer y = 304
integer width = 2976
integer height = 2112
string title = "Búsqueda de Movimiento de Fruta"
end type
global w_busc_spro_movtofrutagranenca_desverd w_busc_spro_movtofrutagranenca_desverd

type variables
datawindowchild  idwc_tipomv,idwc_especie,idwc_especiedet,idwc_planta



end variables

on w_busc_spro_movtofrutagranenca_desverd.create
int iCurrent
call super::create
end on

on w_busc_spro_movtofrutagranenca_desverd.destroy
call super::destroy
end on

event ue_asignacion;istr_Busq.Argum[1]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[2]	= String(dw_1.Object.tpmv_codigo[dw_1.GetRow()])
istr_Busq.Argum[3]	= String(dw_1.Object.mfge_numero[dw_1.GetRow()])
istr_Busq.Argum[4]	= String(dw_1.Object.mfge_fecmov[dw_1.GetRow()])
istr_Busq.Argum[5]	= String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_Busq.Argum[7]	= String(dw_1.Object.tran_codigo[dw_1.GetRow()])
istr_Busq.Argum[8]	= String(dw_1.Object.mfge_guisii[dw_1.GetRow()])
istr_Busq.Argum[9]	= String(dw_1.Object.mfge_estmov[dw_1.GetRow()])
istr_Busq.Argum[10]	= String(dw_1.Object.clie_codigo[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

event open;call super::open;/*
	Argumentos
		istr_Busq.Argum[1]	:	Planta
		istr_Busq.Argum[2]	:	Tipo de Movimiento
		istr_Busq.Argum[3]	:	Estado del Movimiento
		istr_Busq.Argum[4]	:  Fecha de Movimiento
		istr_Busq.Argum[5]   :  Especie
		istr_Busq.Argum[10]	:  Cliente
		istr_Busq.Argum[9]	: Tipo Frío
*/
Integer	li_Cliente

istr_busq	=	Message.PowerObjectParm

li_Cliente	=	Integer(istr_busq.argum[10])
IF tab_1.tabpage_1.cbx_todas.checked = true THEN
	istr_Busq.Argum[5] = string(0)
END IF


is_ordena 	=	'Número:mfge_numero,Fecha Movto.:mfge_fecmov,Especie:espe_codigo'


tab_1.tabpage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

tab_1.tabpage_1.dw_6.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(li_Cliente) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_1.GetChild("espe_codigo", idwc_especiedet)
idwc_especiedet.SetTransObject(sqlca)
IF idwc_especiedet.Retrieve(li_Cliente) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especiedet.InsertRow(0)
ELSE
	idwc_especiedet.SetSort("espe_nombre A")
	idwc_especiedet.Sort()
END IF

if integer(istr_busq.Argum[9]) <> 7  Then
	dw_1.Retrieve(Integer(istr_busq.Argum[1]),&
						Integer(istr_busq.Argum[2]), &
						Integer(istr_busq.Argum[3]), &
						Integer(istr_busq.Argum[5]), &
						Date(istr_busq.Argum[4]),li_Cliente)
else
	dw_1.DataObject ='dw_mues_spro_movtofrutagranenca_atemper'
	dw_1.SetTransObject(sqlca)
	dw_1.Retrieve(Integer(istr_busq.Argum[1]),&
						Integer(istr_busq.Argum[2]), &
						Integer(istr_busq.Argum[3]), &
						Integer(istr_busq.Argum[5]), &
						Date(istr_busq.Argum[4]),li_Cliente,string(istr_busq.Argum[9]))
End if
//dw_1.Retrieve(Integer(istr_busq.Argum[1]),&
//					Integer(istr_busq.Argum[2]), &
//						Integer(istr_busq.Argum[3]), &
//						Integer(istr_busq.Argum[5]), &
//						Date(istr_busq.Argum[4]))
						
istr_Mant.Argumento[1]	=	istr_Busq.Argum[1]
istr_Mant.Argumento[2]	=	istr_Busq.Argum[2]
istr_Mant.Argumento[3]	=	istr_Busq.Argum[3]
istr_Mant.Argumento[4]	=	istr_Busq.Argum[4]
istr_Mant.Argumento[5]	=	istr_Busq.Argum[5]
istr_Mant.Argumento[10]	=	istr_Busq.Argum[10]

tab_1.tabpage_1.dw_4.GetChild("tpmv_codigo", idwc_tipomv)
idwc_tipomv.SetTransObject(SqlCa)

IF idwc_tipomv.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipos de Movimientos")
	idwc_tipomv.InsertRow(0)
ELSE
	idwc_tipomv.SetFilter("tpmv_frugra = 1")
	idwc_tipomv.Filter()
END IF

tab_1.tabpage_1.dw_4.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_6.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_4.Retrieve()
tab_1.tabpage_1.dw_5.InsertRow(0)
tab_1.tabpage_1.dw_6.InsertRow(0)

IF istr_Busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_Busq.Argum[1]))
	tab_1.tabpage_1.dw_5.Enabled	=	false
		END IF

IF istr_Busq.Argum[2] <> "" THEN
	tab_1.tabpage_1.ddlb_estado.Reset()
	
	IF istr_Busq.Argum[2] = '21' OR istr_Busq.Argum[2] = '2' THEN
		tab_1.tabpage_1.ddlb_estado.AddItem('Transitoria')
		tab_1.tabpage_1.ddlb_estado.AddItem('Conf.Packing')
		tab_1.tabpage_1.ddlb_estado.AddItem('Definitiva')
	ELSE
		tab_1.tabpage_1.ddlb_estado.AddItem('Transitoria')
		tab_1.tabpage_1.ddlb_estado.AddItem('Definitiva')
	END IF
	tab_1.tabpage_1.dw_4.SetItem(1, "tpmv_codigo", Integer(istr_Busq.Argum[2]))
	tab_1.tabpage_1.dw_4.Enabled	=	false
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


istr_Busq.Argum[1]		=	""
istr_Busq.Argum[2]		=	""
istr_Busq.Argum[3]		=	""
istr_Busq.Argum[4]		=	""
istr_Busq.Argum[10]		=	""
istr_Busq.Argum[13]		=	""

tab_1.tabpage_1.pb_filtrar.SetFocus()
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_movtofrutagranenca_desverd
boolean visible = false
integer x = 2537
integer y = 1132
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_movtofrutagranenca_desverd
integer x = 197
integer y = 868
integer width = 2181
integer height = 984
integer taborder = 60
string dataobject = "dw_mues_spro_movtofrutagranenca_deverd"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_spro_movtofrutagranenca_desverd
integer x = 2528
integer y = 1428
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_spro_movtofrutagranenca_desverd
integer x = 101
integer y = 80
integer height = 700
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
integer height = 572
long backcolor = 33554431
string text = "Filtros              "
st_1 st_1
st_2 st_2
dw_4 dw_4
dw_5 dw_5
em_fechamovto em_fechamovto
st_4 st_4
st_5 st_5
dw_6 dw_6
st_6 st_6
cbx_todas cbx_todas
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
this.dw_6=create dw_6
this.st_6=create st_6
this.cbx_todas=create cbx_todas
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
this.Control[iCurrent+8]=this.dw_6
this.Control[iCurrent+9]=this.st_6
this.Control[iCurrent+10]=this.cbx_todas
this.Control[iCurrent+11]=this.ddlb_estado
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
destroy(this.dw_6)
destroy(this.st_6)
destroy(this.cbx_todas)
destroy(this.ddlb_estado)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1947
integer y = 332
boolean default = false
long backcolor = 134217742
end type

event pb_filtrar::clicked;call super::clicked;Date	ld_FechaMovto

IF NOT IsDate(em_fechamovto.Text) THEN
	MessageBox("Atención","Falta ingresar la Fecha de Movimiento de Inicio.")
	RETURN
END IF

ld_FechaMovto	=	Date(em_fechamovto.Text)

IF istr_Mant.Argumento[5] = '' THEN
	MessageBox("Atención","Falta ingresar la Especie.")
	RETURN
END IF

if integer(istr_busq.Argum[9]) <> 7 Then
	IF dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
						Integer(istr_Mant.Argumento[2]), &
						Integer(istr_Mant.Argumento[3]), &
						Integer(istr_Mant.Argumento[5]),&
						ld_FechaMovto,Integer(istr_Mant.Argumento[10])) > 0 THEN
					
		dw_1.SetFocus()
		dw_1.SelectRow(1,True)
	ELSE
		MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	END IF
ELSE
	dw_1.DataObject ='dw_mues_spro_movtofrutagranenca_atemper'
	dw_1.SetTransObject(sqlca)
	IF dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
						Integer(istr_Mant.Argumento[2]), &
						Integer(istr_Mant.Argumento[3]), &
						Integer(istr_Mant.Argumento[5]),&
						ld_FechaMovto,Integer(istr_Mant.Argumento[10]),string(istr_busq.Argum[9])) > 0 THEN
						
		dw_1.SetFocus()
		dw_1.SelectRow(1,True)
	ELSE
		MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	END IF
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer height = 572
long backcolor = 16711680
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1952
integer y = 280
long backcolor = 134217748
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
integer height = 448
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
integer height = 448
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer height = 572
long backcolor = 16711680
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
long textcolor = 33554431
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
long textcolor = 33554431
string text = "Número Lote"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1961
integer y = 340
long backcolor = 134217748
end type

type st_1 from statictext within tabpage_1
integer x = 64
integer y = 48
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
integer x = 421
integer y = 160
integer width = 873
integer height = 96
integer taborder = 31
boolean bringtotop = true
boolean enabled = false
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
integer x = 421
integer y = 40
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
		istr_Mant.Argumento[1]	=	data
END CHOOSE
end event

type em_fechamovto from editmask within tabpage_1
integer x = 1733
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
integer x = 1339
integer y = 48
integer width = 393
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
string text = "Movto.Inicial"
boolean focusrectangle = false
end type

type st_5 from statictext within tabpage_1
integer x = 64
integer y = 288
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
string text = "Estado"
boolean focusrectangle = false
end type

type dw_6 from datawindow within tabpage_1
integer x = 421
integer y = 400
integer width = 873
integer height = 96
integer taborder = 20
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
		istr_Mant.Argumento[5]	=	data
END CHOOSE
end event

type st_6 from statictext within tabpage_1
integer x = 64
integer y = 408
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
string text = "Especie"
boolean focusrectangle = false
end type

type cbx_todas from checkbox within tabpage_1
integer x = 1358
integer y = 408
integer width = 302
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;Integer	li_Nula

SetNull(li_Nula)

IF This.Checked THEN
	dw_6.Enabled				=	False
	istr_Mant.Argumento[5]	=	'0'
	dw_6.SetItem(1,"espe_codigo",li_Nula)
ELSE
	dw_6.Enabled				=	True
	istr_Mant.Argumento[5]	=	''
	dw_6.SetFocus()
END IF
end event

type ddlb_estado from dropdownlistbox within tabpage_1
integer x = 421
integer y = 280
integer width = 873
integer height = 324
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

