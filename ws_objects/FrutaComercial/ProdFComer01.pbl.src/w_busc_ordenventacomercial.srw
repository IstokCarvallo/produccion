$PBExportHeader$w_busc_ordenventacomercial.srw
forward
global type w_busc_ordenventacomercial from w_busqueda
end type
end forward

global type w_busc_ordenventacomercial from w_busqueda
integer x = 233
integer y = 304
integer width = 3259
integer height = 1796
string title = "Búsqueda Ordenes de Venta"
boolean resizable = false
end type
global w_busc_ordenventacomercial w_busc_ordenventacomercial

type variables
w_mant_deta_clienprove	iw_mantencion

Integer	ii_tipo
end variables

on w_busc_ordenventacomercial.create
call super::create
end on

on w_busc_ordenventacomercial.destroy
call super::destroy
end on

event open;call super::open;Integer           li_TipoAn

istr_busq			=	Message.PowerobjectParm

istr_mant.dw	=	dw_1

is_ordena		=	'Nº Orden:odfc_numero,Rut Cliente:clpr_rut,Cliente:clpr_nombre,' + &
						'Fecha:odfc_fecham'
					  
IF dw_1.Retrieve(integer(istr_busq.argum[1])) > 0 THEN
	IF istr_busq.argum[2] <> '' THEN
		dw_1.SetFilter("odfc_tipret =" + istr_busq.argum[2])
	   dw_1.Filter()
	END IF	
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	istr_busq.argum[2] = ""
	CloseWithReturn(This,istr_busq)
END IF
end event

event ue_asignacion();istr_busq.argum[1]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"plde_codigo"))
istr_busq.argum[2]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"odfc_numero"))
istr_busq.argum[3]	= dw_1.GetItemString(dw_1.GetRow(),"clpr_rut")
istr_busq.argum[4]	= dw_1.GetItemString(dw_1.GetRow(),"clpr_nombre")
istr_busq.argum[5]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"odfc_tipret"))
istr_busq.argum[6]	= dw_1.GetItemString(dw_1.GetRow(),"odfc_observ")

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_ordenventacomercial
boolean visible = false
integer x = 2862
integer y = 1004
integer taborder = 50
boolean enabled = false
end type

event pb_insertar::clicked;call super::clicked;istr_mant.argumento[1] = ""
OpenWithParm(iw_mantencion, istr_mant)

Parent.TriggerEvent("ue_grabacion")
end event

type dw_1 from w_busqueda`dw_1 within w_busc_ordenventacomercial
integer x = 82
integer y = 724
integer width = 2670
integer height = 876
integer taborder = 60
string dataobject = "dw_mues_ordenventacomenca"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_ordenventacomercial
integer x = 2862
integer y = 1388
end type

event pb_salir::clicked;
istr_busq.argum[2] = ""
istr_busq.argum[3] = ""

CloseWithReturn(Parent,istr_busq)

Return 1
end event

type tab_1 from w_busqueda`tab_1 within w_busc_ordenventacomercial
integer width = 2674
integer selectedtab = 2
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
boolean visible = false
integer width = 2638
boolean enabled = false
string text = "Filtros              "
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
boolean visible = false
integer x = 1280
boolean enabled = false
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2638
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2139
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2638
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 759
integer width = 1326
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "clpr_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer width = 699
string text = "Nombre Cliente"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 759
integer width = 389
end type

event sle_argumento1::getfocus;call super::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= TRUE
is_busca							= "odfc_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
string text = "Nro. Orden"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2144
integer y = 312
end type

