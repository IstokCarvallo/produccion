$PBExportHeader$w_busc_secuenciaprograma.srw
forward
global type w_busc_secuenciaprograma from w_busqueda
end type
end forward

global type w_busc_secuenciaprograma from w_busqueda
integer x = 78
integer y = 176
integer width = 1669
integer height = 1108
string title = "Búsqueda de Programas"
end type
global w_busc_secuenciaprograma w_busc_secuenciaprograma

type variables
DataWindowChild	dw_especies
end variables

event open;Long		ll_fila = 1

istr_busq 				= Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

is_ordena = 'Secuencia:secuencia'

TriggerEvent("ue_ordenamiento")

IF UpperBound(istr_busq.argum) > 0 THEN
	IF istr_busq.argum[1] <> "" THEN
		
		IF dw_1.Retrieve(Integer(istr_busq.argum[1]), Integer(istr_busq.argum[3]),&
			Integer(istr_busq.argum[4]),Integer(istr_busq.argum[2])) > 0 THEN
			dw_1.SetFocus()
			dw_1.SelectRow(1,True)
		ELSE
			MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
			CloseWithReturn(This, istr_busq)
		END IF
	ELSE
		CloseWithReturn(This, istr_busq)
	END IF
ELSE
	CloseWithReturn(This, istr_busq)
END IF
end event

on w_busc_secuenciaprograma.create
call super::create
end on

on w_busc_secuenciaprograma.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[1]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"prsd_secuen"))
//istr_busq.argum[2]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"tpem_cancaj"))


CloseWithReturn(This,istr_busq)
end event

event resize;//
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_secuenciaprograma
boolean visible = false
integer x = 1646
integer y = 308
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_secuenciaprograma
integer x = 64
integer y = 56
integer width = 1143
integer taborder = 60
string dataobject = "dw_mant_mues_spro_programasalidasdeta_2"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_secuenciaprograma
integer x = 1339
integer y = 716
end type

event pb_salir::clicked;istr_busq.argum[2] = ""
istr_busq.argum[3] = ""
istr_busq.argum[4] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_secuenciaprograma
boolean visible = false
integer width = 1422
integer height = 620
boolean fixedwidth = true
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
boolean visible = false
integer width = 1385
integer height = 492
string text = "Filtros"
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
boolean visible = false
integer x = 1083
integer y = 332
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
	
	istr_busq.argum[3]	= String(dw_1.GetItemNumber(1, "vari_codigo"))
	istr_busq.argum[4]	= dw_1.GetItemString(1, "vari_nombre")
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
//	IF tab_1.tabpage_1.dw_especie.Enabled THEN
//		tab_1.tabpage_1.dw_especie.SetFocus()
//	END IF
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 1385
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
integer width = 1385
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

