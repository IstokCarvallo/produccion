$PBExportHeader$w_busc_spro_concalprecos.srw
$PBExportComments$Para la busqueda de los parametros de PreCosecha en la tabla spro_concalprecos
forward
global type w_busc_spro_concalprecos from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_4 from datawindow within tabpage_1
end type
type dw_6 from datawindow within tabpage_1
end type
type dw_5 from datawindow within tabpage_1
end type
type st_4 from statictext within tabpage_1
end type
type st_6 from statictext within tabpage_1
end type
type dw_7 from datawindow within tabpage_1
end type
type cbx_todas_espe from checkbox within tabpage_1
end type
type cbx_todas_vari from checkbox within tabpage_1
end type
type cbx_todos_prod from checkbox within tabpage_1
end type
type st_3 from st_argum2 within tabpage_3
end type
type sle_argumento3 from sle_argumento2 within tabpage_3
end type
end forward

global type w_busc_spro_concalprecos from w_busqueda
integer x = 123
integer y = 304
integer width = 2926
integer height = 1944
string title = "Búsqueda de Parametros de PreCosecha"
end type
global w_busc_spro_concalprecos w_busc_spro_concalprecos

type variables
Integer	ii_posicion
datawindowchild  idwc_especie, idwc_planta, idwc_productor, idwc_variedad


end variables

on w_busc_spro_concalprecos.create
int iCurrent
call super::create
end on

on w_busc_spro_concalprecos.destroy
call super::destroy
end on

event open;call super::open;istr_busq	=	Message.PowerObjectParm

is_ordena 	= 'Especie:espe_codigo,Variedad:vari_codigo,Productor:prod_codigo'

tab_1.tabpage_1.dw_7.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_7.InsertRow(0)
tab_1.tabpage_1.dw_7.Setitem(1,"plde_codigo",gstr_ParamPlanta.CodigoPlanta)

tab_1.tabpage_1.dw_5.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(SqlCa)
IF idwc_variedad.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
END IF

tab_1.tabpage_1.dw_4.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_6.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_4.InsertRow(0)
tab_1.tabpage_1.dw_5.InsertRow(0)
tab_1.tabpage_1.dw_6.InsertRow(0)

IF istr_busq.Argum[2] <> "" THEN
	tab_1.tabpage_1.dw_6.SetItem(1,"espe_codigo",Integer(istr_busq.Argum[2]))
ELSE	
	tab_1.tabpage_1.cbx_todas_espe.checked = True
END IF

IF istr_busq.Argum[3] <> "" THEN
	tab_1.tabpage_1.dw_5.SetItem(1,"vari_codigo",Integer(istr_busq.Argum[3]))
ELSE	
	tab_1.tabpage_1.cbx_todas_vari.checked = True
END IF

IF istr_busq.Argum[4] <> "" THEN
	tab_1.tabpage_1.dw_4.SetItem(1,"prod_codigo",Long(istr_busq.Argum[4]))
ELSE	
	tab_1.tabpage_1.cbx_todos_prod.checked = True
END IF

tab_1.tabpage_1.dw_6.SetFocus()
end event

event ue_asignacion();istr_busq.argum[1]	= String(gstr_ParamPlanta.CodigoPlanta)
istr_busq.argum[2]	= String(dw_1.Object.ccpr_folio[ii_posicion])

CloseWithReturn(This,istr_busq)
end event

event resize;call super::resize;Long		maximo

IF tab_1.Width > dw_1.Width THEN
	maximo = tab_1.Width
ELSE
	maximo = dw_1.Width
END IF

This.Width		= maximo + 540
//gb_1.x 			= This.WorkSpaceWidth() - 351
//gb_1.y			= This.WorkSpaceHeight() - 348
//gb_2.x			= gb_1.x
//gb_2.y			= gb_1.y - 304
pb_salir.x		= This.WorkSpaceWidth() - 292
pb_salir.y		= This.WorkSpaceHeight() - 264
pb_insertar.x	= This.WorkSpaceWidth() - 292
pb_insertar.y	= pb_salir.y - 304
tab_1.x			= 78
tab_1.y			= 69
dw_1.x			= 78
dw_1.y			= tab_1.Height + 136

end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_concalprecos
boolean visible = false
integer x = 2601
integer y = 1068
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_concalprecos
integer y = 904
integer width = 2395
integer taborder = 60
string dataobject = "dw_mues_concalprecos"
end type

event dw_1::doubleclicked;call super::doubleclicked;ii_posicion = row
end event

type pb_salir from w_busqueda`pb_salir within w_busc_spro_concalprecos
integer x = 2601
integer y = 1360
end type

event pb_salir::clicked;
istr_busq.argum[3] = ""
istr_busq.argum[4] = ""
istr_busq.argum[5] = ""
istr_busq.argum[6] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_spro_concalprecos
integer x = 87
integer y = 80
integer width = 2007
integer height = 736
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 1970
integer height = 608
string text = "Filtros              "
st_1 st_1
st_2 st_2
dw_4 dw_4
dw_6 dw_6
dw_5 dw_5
st_4 st_4
st_6 st_6
dw_7 dw_7
cbx_todas_espe cbx_todas_espe
cbx_todas_vari cbx_todas_vari
cbx_todos_prod cbx_todos_prod
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.dw_4=create dw_4
this.dw_6=create dw_6
this.dw_5=create dw_5
this.st_4=create st_4
this.st_6=create st_6
this.dw_7=create dw_7
this.cbx_todas_espe=create cbx_todas_espe
this.cbx_todas_vari=create cbx_todas_vari
this.cbx_todos_prod=create cbx_todos_prod
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.dw_6
this.Control[iCurrent+5]=this.dw_5
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.dw_7
this.Control[iCurrent+9]=this.cbx_todas_espe
this.Control[iCurrent+10]=this.cbx_todas_vari
this.Control[iCurrent+11]=this.cbx_todos_prod
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_4)
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.dw_7)
destroy(this.cbx_todas_espe)
destroy(this.cbx_todas_vari)
destroy(this.cbx_todos_prod)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1760
integer y = 448
boolean default = false
end type

event pb_filtrar::clicked;
Tab_1.tabpage_1.dw_4.accepttext()
Tab_1.tabpage_1.dw_5.accepttext()
Tab_1.tabpage_1.dw_6.accepttext()

IF String(Tab_1.tabpage_1.dw_6.object.espe_codigo[1]) = "" or &
	Isnull(Tab_1.tabpage_1.dw_6.object.espe_codigo[1]) THEN
	istr_busq.argum[2] = "0"
END IF

IF String(Tab_1.tabpage_1.dw_5.object.vari_codigo[1]) = "" or &
	Isnull(Tab_1.tabpage_1.dw_5.object.vari_codigo[1]) THEN
	istr_busq.argum[3] = "0"
END IF

IF String(Tab_1.tabpage_1.dw_4.object.prod_codigo[1]) = "" or &
	Isnull(Tab_1.tabpage_1.dw_4.object.prod_codigo[1]) THEN
	istr_busq.argum[4] = "0"
END IF


IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Long(istr_busq.argum[4]) , &
					  Integer(istr_busq.argum[2]),Integer(istr_busq.argum[3])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF

end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 1970
integer height = 608
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1760
integer y = 312
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 1970
integer height = 608
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
is_busca							= "vari_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 645
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
is_busca							= "ccpr_folio"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer width = 407
string text = "Número Folio"
boolean focusrectangle = true
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1760
integer y = 300
end type

type st_1 from statictext within tabpage_1
integer x = 82
integer y = 216
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_1
integer x = 82
integer y = 496
integer width = 297
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type dw_4 from datawindow within tabpage_1
integer x = 485
integer y = 480
integer width = 873
integer height = 96
integer taborder = 31
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "prod_codigo"
		istr_busq.Argum[4]	=	data
END CHOOSE
end event

type dw_6 from datawindow within tabpage_1
integer x = 485
integer y = 208
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

event itemchanged;dw_1.accepttext()
CHOOSE CASE dwo.Name
	CASE "espe_codigo"
		istr_busq.Argum[2]	=	data
END CHOOSE

Tab_1.Tabpage_1.dw_5.enabled = True
dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_especie.Retrieve(istr_busq.Argum[2])

end event

type dw_5 from datawindow within tabpage_1
integer x = 485
integer y = 348
integer width = 873
integer height = 96
integer taborder = 25
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name
	CASE "vari_codigo"
		istr_busq.Argum[3]	=	data
END CHOOSE
end event

type st_4 from statictext within tabpage_1
integer x = 82
integer y = 364
integer width = 297
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type st_6 from statictext within tabpage_1
integer x = 82
integer y = 80
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

type dw_7 from datawindow within tabpage_1
integer x = 485
integer y = 68
integer width = 873
integer height = 96
integer taborder = 25
boolean enabled = false
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;dw_1.accepttext()
CHOOSE CASE dwo.Name
	CASE "espe_codigo"
		istr_busq.Argum[1]	=	data
END CHOOSE
end event

type cbx_todas_espe from checkbox within tabpage_1
integer x = 1399
integer y = 220
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
end type

event clicked;IF cbx_todas_espe.checked = false THEN
	tab_1.tabpage_1.dw_6.enabled = true
ELSE
	tab_1.tabpage_1.dw_6.enabled = false
	tab_1.tabpage_1.dw_6.insertrow(1)
END IF
end event

type cbx_todas_vari from checkbox within tabpage_1
integer x = 1399
integer y = 356
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
end type

event clicked;IF cbx_todas_vari.checked = false THEN
	tab_1.tabpage_1.dw_5.enabled = true
ELSE
	tab_1.tabpage_1.dw_5.enabled = false
	tab_1.tabpage_1.dw_5.insertrow(1)
END IF
end event

type cbx_todos_prod from checkbox within tabpage_1
integer x = 1399
integer y = 484
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
end type

event clicked;IF cbx_todos_prod.checked = false THEN
	tab_1.tabpage_1.dw_4.enabled = true
ELSE
	tab_1.tabpage_1.dw_4.enabled = false
	tab_1.tabpage_1.dw_4.insertrow(1)
END IF
end event

type st_3 from st_argum2 within tabpage_3
integer y = 328
boolean bringtotop = true
string text = "Productor"
end type

type sle_argumento3 from sle_argumento2 within tabpage_3
integer y = 320
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

