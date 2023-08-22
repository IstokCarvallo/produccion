$PBExportHeader$w_busc_tipopallets.srw
forward
global type w_busc_tipopallets from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_especie from datawindow within tabpage_1
end type
end forward

global type w_busc_tipopallets from w_busqueda
integer x = 78
integer y = 176
integer width = 2217
integer height = 1768
string title = "Búsqueda de Variedades"
end type
global w_busc_tipopallets w_busc_tipopallets

type variables
DataWindowChild	dw_especies
end variables

event open;/*
				istr_busq.argum[1]	= cliente
				istr_busq.argum[2]	= embalaje
				istr_busq.argum[3]	= tpem_codigo
				istr_busq.argum[4]	= tpem_cancaj
*/
Long		ll_fila = 1


istr_busq 				= Message.PowerObjectParm
istr_busq.argum[3]	= ""
istr_busq.argum[4]	= ""

tab_1.tabpage_1.dw_especie.GetChild("emba_codigo", dw_especies)

dw_especies.SetTransObject(sqlca)
dw_especies.Retrieve(0, 0, Integer(istr_busq.argum[1]))

dw_1.SetTransObject(sqlca)

tab_1.tabpage_1.dw_especie.SetTransObject(sqlca)
tab_1.tabpage_1.dw_especie.InsertRow(0)

dw_1.SetRowFocusIndicator(Hand!)




is_ordena = 'Código:tpem_codigo,Cajas:tpen_cancaj'

TriggerEvent("ue_ordenamiento")

IF UpperBound(istr_busq.argum) > 0 THEN
	IF istr_busq.argum[1] <> "" THEN
		tab_1.tabpage_1.dw_especie.SetItem(1, "emba_codigo", Integer(istr_busq.argum[2]))
		tab_1.tabpage_1.dw_especie.SetRow(ll_fila)
		tab_1.tabpage_1.dw_especie.ScrollToRow(ll_fila)
		tab_1.tabpage_1.dw_especie.SetFocus()
		
      IF istr_busq.argum[2] = "D" THEN tab_1.tabpage_1.dw_especie.Enabled = FALSE
		
		IF dw_1.Retrieve(Integer(istr_busq.argum[1]), istr_busq.argum[2]) > 0 THEN
			tab_1.tabpage_1.dw_especie.Enabled = False
			dw_1.SetFocus()
			dw_1.SelectRow(1,True)
		ELSE
			MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
			CloseWithReturn(This, istr_busq)
		END IF
	ELSE
//		istr_busq.argum[1] = String(tab_1.tabpage_1.dw_especie.GetItemNumber(1, "espe_codigo"))
		CloseWithReturn(This, istr_busq)
	END IF
ELSE
//	istr_busq.argum[1] = String(tab_1.tabpage_1.dw_especie.GetItemNumber(1, "espe_codigo"))
	CloseWithReturn(This, istr_busq)
END IF
end event

on w_busc_tipopallets.create
int iCurrent
call super::create
end on

on w_busc_tipopallets.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[3]	= dw_1.GetItemString(dw_1.GetRow(),"tpem_codigo")
istr_busq.argum[4]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"tpem_cancaj"))

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_tipopallets
integer x = 1920
integer y = 1160
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_tipopallets
integer y = 760
integer width = 1751
integer taborder = 60
string dataobject = "dw_mues_tipopallemba"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_tipopallets
integer x = 1920
integer y = 1444
end type

event pb_salir::clicked;istr_busq.argum[2] = ""
istr_busq.argum[3] = ""
istr_busq.argum[4] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_tipopallets
integer width = 1751
integer height = 620
boolean fixedwidth = true
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 1714
integer height = 492
string text = "Filtros"
st_1 st_1
dw_especie dw_especie
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_especie=create dw_especie
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_especie
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_especie)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1449
integer y = 332
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
	
	istr_busq.argum[3]	= String(dw_1.GetItemNumber(1, "vari_codigo"))
	istr_busq.argum[4]	= dw_1.GetItemString(1, "vari_nombre")
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	IF tab_1.tabpage_1.dw_especie.Enabled THEN
		tab_1.tabpage_1.dw_especie.SetFocus()
	END IF
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 1714
integer height = 492
string text = "Ordenamiento"
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 745
integer width = 960
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer x = 9
integer width = 727
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 1714
integer height = 492
string text = "Búsqueda"
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
end type

event sle_argumento2::getfocus;call super::getfocus;This.SelectText(1, Len(This.Text))

This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer width = 187
end type

event sle_argumento1::getfocus;call super::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "vari_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
string text = "Código"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1522
integer y = 324
end type

type st_1 from statictext within tabpage_1
integer x = 105
integer y = 200
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type dw_especie from datawindow within tabpage_1
integer x = 398
integer y = 184
integer width = 869
integer height = 92
integer taborder = 11
boolean bringtotop = true
string dataobject = "dddw_embalajes"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_busq.argum[1] = data
istr_busq.argum[2] = dw_especies.GetItemString(dw_especies.GetRow(), "espe_nombre")
end event

