$PBExportHeader$w_busc_spro_ccalidadpackenca.srw
$PBExportComments$Ventana de Busqueda de Control de Calidad Packing.
forward
global type w_busc_spro_ccalidadpackenca from w_busqueda
end type
type dw_5 from datawindow within tabpage_1
end type
type st_1 from statictext within tabpage_1
end type
end forward

global type w_busc_spro_ccalidadpackenca from w_busqueda
string title = "Busqueda de Control Calidad Packing"
end type
global w_busc_spro_ccalidadpackenca w_busc_spro_ccalidadpackenca

type variables
DataWindowChild	idwc_planta, idwc_especie, idwc_productor
end variables

event open;call super::open;/*
Argumentos
istr_Busq.Argum[1]	:	Planta
*/

x	=	58
y	=	200

istr_busq	=	Message.PowerObjectParm
is_ordena 	=	'Número Folio:ccap_folio,Fecha Control:ccap_feccon,Tipo Orden:ccap_tipord,Nro. Orden:ccap_nrodoc'

istr_Busq.Argum[2]	=	""

tab_1.tabpage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)

//Especie
dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especie.InsertRow(0)
END IF

//Productor
dw_1.GetChild("prod_codigo",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
IF idwc_productor.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
ELSE
	idwc_productor.InsertRow(0)
END IF

IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
END IF

tab_1.tabpage_1.dw_5.SetTransObject(SQLCA)
tab_1.tabpage_1.dw_5.InsertRow(0)

IF istr_Busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_Busq.Argum[1]))
	tab_1.tabpage_1.dw_5.Enabled	=	False
	tab_1.tabpage_1.pb_filtrar.PostEvent(Clicked!)
END IF

tab_1.tabpage_1.pb_filtrar.SetFocus()
end event

on w_busc_spro_ccalidadpackenca.create
int iCurrent
call super::create
end on

on w_busc_spro_ccalidadpackenca.destroy
call super::destroy
end on

event ue_asignacion();istr_Busq.Argum[1]	=	String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[2]	=	String(dw_1.Object.ccap_folio[dw_1.GetRow()])
istr_Busq.Argum[3]	=	String(dw_1.Object.ccap_tipord[dw_1.GetRow()])
istr_Busq.Argum[4]	=	String(dw_1.Object.ccap_nrodoc[dw_1.GetRow()])
istr_Busq.Argum[5]	=	String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_Busq.Argum[6]	=	String(dw_1.Object.vari_codigo[dw_1.GetRow()])
istr_Busq.Argum[7]	=	String(dw_1.Object.prod_codigo[dw_1.GetRow()])
istr_Busq.Argum[8]	=	String(dw_1.Object.line_codigo[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_spro_ccalidadpackenca
boolean visible = false
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_spro_ccalidadpackenca
string dataobject = "dw_mues_spro_ccalidadpackenca"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_spro_ccalidadpackenca
end type

type tab_1 from w_busqueda`tab_1 within w_busc_spro_ccalidadpackenca
integer x = 73
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
dw_5 dw_5
st_1 st_1
end type

on tabpage_1.create
this.dw_5=create dw_5
this.st_1=create st_1
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_5
this.Control[iCurrent+2]=this.st_1
end on

on tabpage_1.destroy
call super::destroy
destroy(this.dw_5)
destroy(this.st_1)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_Busq.Argum[1]))> 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
integer x = 498
integer y = 240
long backcolor = 12632256
end type

event sle_argumento2::modified;call super::modified;This.SelectText(1, Len(This.Text))


This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "prod_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
string text = ""
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 498
integer y = 136
integer width = 311
end type

event sle_argumento1::modified;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0

es_numero						= True
is_busca							= "ccap_nrodoc"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer y = 144
integer width = 407
string text = "Número Folio"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
end type

event pb_buscar::clicked;Long		ll_fila
Boolean	lb_inicio, lb_termino

dw_1.SetRedraw(False)

IF dw_1.RowCount() > 0 THEN
	dw_1.SetSort(is_busca + " A")
	dw_1.Sort()
	dw_1.SetFilter("")
	dw_1.Filter()

	tab_1.tabpage_2.dw_3.Reset()
	
	IF es_numero THEN
		ll_fila	=	dw_1.Find(is_busca + " >= " + is_argume, 1, dw_1.RowCount())
	ELSE
		IF Mid(is_argume, 1, 1) = "%" THEN
			lb_inicio	=	True
			is_argume	=	Mid(is_argume, 2)
		END IF
		
		IF Mid(is_argume, Len(is_argume)) = "%" THEN
			lb_termino	=	True
			is_argume	=	Mid(is_argume, 1, Len(is_argume) - 1)
		END IF
		
		IF Not lb_inicio THEN
			is_busca	=	"Mid(String(" + is_busca + "), 1, " + String(Len(is_argume)) + ")"
			ll_fila	=	dw_1.Find(is_busca + " >= '" + is_argume + "'", 1, dw_1.RowCount())
		ELSEIF lb_inicio THEN
			Is_busca	=	"Pos(Lower(String(" + is_busca + ")), '" + Lower(is_argume) + "') > 0"
			dw_1.SetFilter(Is_busca)
			dw_1.Filter()
		END IF
	END IF

	IF ll_fila = 0 THEN ll_fila = 1
	
	dw_1.SetRow(ll_fila)
	dw_1.ScrollToRow(ll_fila)
	dw_1.SelectRow(0,False)
	dw_1.SelectRow(ll_fila,True)
	dw_1.SetFocus()
	dw_1.SetRedraw(True)
END IF
end event

type dw_5 from datawindow within tabpage_1
integer x = 489
integer y = 176
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
		istr_Mant.Argumento[1]	=	data
END CHOOSE
end event

type st_1 from statictext within tabpage_1
integer x = 137
integer y = 184
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

