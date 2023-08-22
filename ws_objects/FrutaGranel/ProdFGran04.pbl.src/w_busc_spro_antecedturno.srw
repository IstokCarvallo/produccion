$PBExportHeader$w_busc_spro_antecedturno.srw
$PBExportComments$Ventana de Busqueda Antecedentes de Turno.
forward
global type w_busc_spro_antecedturno from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type dw_especie from datawindow within tabpage_1
end type
type dw_planta from datawindow within tabpage_1
end type
type dw_linea from datawindow within tabpage_1
end type
type st_4 from statictext within tabpage_1
end type
end forward

global type w_busc_spro_antecedturno from w_busqueda
integer x = 123
integer y = 304
integer width = 2926
string title = "Búsqueda de Antecedente Turno"
end type
global w_busc_spro_antecedturno w_busc_spro_antecedturno

type variables
DataWindowChild  idwc_especie, idwc_planta, idwc_linea, idwc_linea1
end variables

on w_busc_spro_antecedturno.create
int iCurrent
call super::create
end on

on w_busc_spro_antecedturno.destroy
call super::destroy
end on

event open;call super::open;istr_busq	=	Message.PowerObjectParm

istr_busq.Argum[13]	=	""

is_ordena = 'Variedad:vari_codigo,Turno:antu_turno,Fecha Proceso:antu_fecpro,Tipo Proceso:antu_tipdoc,Empresa:expo_codigo'

tab_1.tabpage_1.dw_especie.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_planta.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_linea.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_especie.InsertRow(0)
END IF

tab_1.tabpage_1.dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

tab_1.tabpage_1.dw_linea.GetChild("line_codigo", idwc_linea)
idwc_linea.SetTransObject(SqlCa)
IF idwc_linea.Retrieve(Integer(istr_Busq.Argum[1])) = 0 THEN
	MessageBox("Atención","Falta Registrar Linea de Packing")
	idwc_linea.InsertRow(0)
END IF

tab_1.tabpage_1.dw_especie.InsertRow(0)
tab_1.tabpage_1.dw_planta.InsertRow(0)
tab_1.tabpage_1.dw_linea.InsertRow(0)

IF istr_busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_planta.SetItem(1,"plde_codigo",Integer(istr_busq.Argum[1]))
END IF

IF istr_busq.Argum[2] <> "" THEN
	tab_1.tabpage_1.dw_especie.SetItem(1,"espe_codigo",Integer(istr_busq.Argum[2]))
END IF

IF istr_busq.Argum[3] <> "" THEN
	tab_1.tabpage_1.dw_linea.SetItem(1,"line_codigo",Integer(istr_busq.Argum[3]))
END IF

tab_1.tabpage_1.pb_filtrar.SetFocus()
end event

event ue_asignacion();istr_busq.argum[1]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_busq.argum[2]	= String(dw_1.Object.line_codigo[dw_1.GetRow()])
istr_busq.argum[3]	= String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_busq.argum[4]	= String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_busq.argum[5]	= String(dw_1.Object.antu_turno[dw_1.GetRow()])
istr_busq.argum[6]	= String(dw_1.Object.antu_fecpro[dw_1.GetRow()])
istr_busq.argum[7]	= String(dw_1.Object.antu_tipdoc[dw_1.GetRow()])
istr_busq.argum[8]	= String(dw_1.Object.expo_codigo[dw_1.GetRow()])
istr_busq.argum[9]	= String(dw_1.Object.antu_horini[dw_1.GetRow()])
istr_busq.argum[10]	= String(dw_1.Object.antu_horter[dw_1.GetRow()])
istr_busq.argum[11]	= String(dw_1.Object.antu_hoexin[dw_1.GetRow()])
istr_busq.argum[12]	= String(dw_1.Object.antu_hoexte[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_antecedturno
boolean visible = false
integer x = 2601
integer y = 1068
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_antecedturno
integer x = 82
integer y = 740
integer width = 2395
integer taborder = 60
string dataobject = "dw_mues_spro_antecedturno"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_spro_antecedturno
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

type tab_1 from w_busqueda`tab_1 within w_busc_spro_antecedturno
integer x = 101
integer y = 80
integer width = 1993
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 1957
string text = "Filtros              "
st_1 st_1
st_2 st_2
dw_especie dw_especie
dw_planta dw_planta
dw_linea dw_linea
st_4 st_4
end type

on tabpage_1.create
this.st_1=create st_1
this.st_2=create st_2
this.dw_especie=create dw_especie
this.dw_planta=create dw_planta
this.dw_linea=create dw_linea
this.st_4=create st_4
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_especie
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.dw_linea
this.Control[iCurrent+6]=this.st_4
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_especie)
destroy(this.dw_planta)
destroy(this.dw_linea)
destroy(this.st_4)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1577
integer y = 312
integer taborder = 40
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Integer(istr_busq.argum[2]), &
					  Integer(istr_busq.argum[3])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF

end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 1957
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
integer width = 1957
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 645
integer y = 296
long backcolor = 12632256
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
integer y = 304
string text = "Variedad"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 645
integer y = 176
integer width = 219
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "expo_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer y = 184
integer width = 389
string text = "Empresa"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1760
integer y = 300
end type

type st_1 from statictext within tabpage_1
integer x = 64
integer y = 84
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
integer y = 208
integer width = 256
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
string text = "Especie"
boolean focusrectangle = false
end type

type dw_especie from datawindow within tabpage_1
integer x = 507
integer y = 192
integer width = 873
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name

	CASE "espe_codigo"
		istr_busq.Argum[2]	=	data

END CHOOSE
end event

type dw_planta from datawindow within tabpage_1
integer x = 507
integer y = 76
integer width = 873
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name

	CASE "plde_codigo"
		istr_busq.Argum[1]	=	data

END CHOOSE
end event

type dw_linea from datawindow within tabpage_1
integer x = 507
integer y = 304
integer width = 873
integer height = 96
integer taborder = 30
boolean bringtotop = true
string dataobject = "dddw_lineapacking"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;CHOOSE CASE dwo.Name

	CASE "line_codigo"
		istr_busq.Argum[3]	=	data

END CHOOSE
end event

type st_4 from statictext within tabpage_1
integer x = 64
integer y = 320
integer width = 430
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
string text = "Linea Packing"
boolean focusrectangle = false
end type

