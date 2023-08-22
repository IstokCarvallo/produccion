$PBExportHeader$w_busc_calicosechero.srw
$PBExportComments$Ventana de Busqueda  de Movimientos de Fruta Comercial.
forward
global type w_busc_calicosechero from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_4 from datawindow within tabpage_1
end type
type dw_5 from datawindow within tabpage_1
end type
type st_5 from statictext within tabpage_1
end type
type dw_6 from datawindow within tabpage_1
end type
type st_3 from st_argum2 within tabpage_3
end type
type sle_argumento3 from sle_argumento2 within tabpage_3
end type
end forward

global type w_busc_calicosechero from w_busqueda
integer x = 123
integer y = 304
integer width = 3145
string title = "Búsqueda de Calidad Fruta Comercial"
end type
global w_busc_calicosechero w_busc_calicosechero

type variables
datawindowchild  idwc_tipo, idwc_envase, idwc_calidad

end variables

on w_busc_calicosechero.create
int iCurrent
call super::create
end on

on w_busc_calicosechero.destroy
call super::destroy
end on

event ue_asignacion;istr_Busq.Argum[3]	= dw_1.Object.cale_calida[dw_1.GetRow()]
istr_Busq.Argum[4]	= dw_1.Object.cale_nombre[dw_1.GetRow()]
istr_Busq.Argum[5]   = ""

CloseWithReturn(This,istr_busq)
end event

event open;call super::open;String ls_calidad,ls_tipo, ls_codigo
/*
	Argumentos
		istr_Busq.Argum[1]	:	Tipo envase
		istr_Busq.Argum[2]	:	Código envase
		istr_Busq.Argum[3]	:	Calidad                                                                                    
*/

istr_busq	=	Message.PowerObjectParm
is_ordena 	=	'Tipo Envase:enva_tipoen,Código Envase:enva_codigo,Calidad:cale_calida'

tab_1.tabpage_1.dw_5.GetChild("enva_tipoen", idwc_tipo)
idwc_tipo.SetTransObject(SqlCa)

tab_1.tabpage_1.dw_4.GetChild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(SqlCa)
IF idwc_envase.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
	idwc_envase.InsertRow(0)
END IF

tab_1.tabpage_1.dw_6.GetChild("cale_calida", idwc_calidad)
idwc_calidad.SetTransObject(SqlCa)
IF idwc_calidad.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]),&
                         istr_busq.argum[3]) > 0 THEN
	idwc_calidad.InsertRow(0)
END IF

tab_1.tabpage_1.dw_4.SetTransObject(SqlCa)
tab_1.tabpage_1.dw_5.SetTransObject(SqlCa)
tab_1.tabpage_1.dw_6.SetTransObject(SqlCa)

tab_1.tabpage_1.dw_4.InsertRow(0)
tab_1.tabpage_1.dw_5.InsertRow(0)
tab_1.tabpage_1.dw_6.InsertRow(0)

ls_tipo    = istr_Busq.Argum[1]
ls_codigo  = istr_Busq.Argum[2]
ls_calidad = istr_Busq.Argum[3]

IF ls_tipo = '0' THEN ls_tipo = ""
IF ls_codigo = '0' THEN ls_codigo = ""
IF ls_calidad = '*' THEN ls_calidad = ""

tab_1.tabpage_1.dw_5.SetItem(1, "enva_tipoen", ls_tipo)
tab_1.tabpage_1.dw_5.Enabled	=	False

tab_1.tabpage_1.dw_4.SetItem(1, "enva_codigo", ls_codigo)
tab_1.tabpage_1.dw_4.Enabled	=	False

tab_1.tabpage_1.dw_6.SetItem(1, "cale_calida", ls_calidad)
tab_1.tabpage_1.dw_6.Enabled	=	False

tab_1.tabpage_1.pb_filtrar.TriggerEvent(Clicked!)

end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_calicosechero
boolean visible = false
integer x = 2629
integer y = 952
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_calicosechero
integer x = 101
integer y = 656
integer width = 2391
integer height = 948
integer taborder = 60
string dataobject = "dw_mues_calicosechero"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_calicosechero
integer x = 2601
integer y = 1276
end type

event pb_salir::clicked;istr_busq.argum[1] = ""
istr_busq.argum[2] = ""
istr_busq.argum[3] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_calicosechero
integer x = 101
integer y = 80
integer width = 2386
integer height = 544
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
integer width = 2350
integer height = 416
string text = "Filtros              "
st_1 st_1
st_2 st_2
dw_4 dw_4
dw_5 dw_5
st_5 st_5
dw_6 dw_6
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.dw_4=create dw_4
this.dw_5=create dw_5
this.st_5=create st_5
this.dw_6=create dw_6
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.dw_6
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.st_5)
destroy(this.dw_6)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2071
integer y = 228
boolean default = false
end type

event pb_filtrar::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]),&
                 istr_busq.argum[3]) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2350
integer height = 416
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2080
integer y = 204
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
integer width = 2350
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
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= rgb(166,180,210)
sle_argumento1.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= rgb(166,180,210)
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
sle_argumento2.BackColor	= rgb(166,180,210)
sle_argumento2.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= rgb(166,180,210)
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
integer x = 2094
integer y = 216
end type

type st_1 from statictext within tabpage_1
integer x = 64
integer y = 52
integer width = 366
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Tipo Envase"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 64
integer y = 168
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Cod. Envase"
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
string dataobject = "dddw_envase"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within tabpage_1
integer x = 489
integer y = 40
integer width = 873
integer height = 96
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_tipoenvase"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
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
long textcolor = 8388608
long backcolor = 553648127
string text = "Calidad"
boolean focusrectangle = false
end type

type dw_6 from datawindow within tabpage_1
integer x = 489
integer y = 280
integer width = 896
integer height = 112
integer taborder = 35
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_calicocechero"
boolean border = false
boolean livescroll = true
end type

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
sle_argumento1.BackColor	= rgb(166,180,210)
sle_argumento1.TabOrder		= 0
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= rgb(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "tran_codigo"
end event

