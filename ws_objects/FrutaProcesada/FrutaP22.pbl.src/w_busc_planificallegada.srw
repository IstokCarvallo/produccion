$PBExportHeader$w_busc_planificallegada.srw
forward
global type w_busc_planificallegada from w_busqueda
end type
end forward

global type w_busc_planificallegada from w_busqueda
integer x = 78
integer y = 176
integer width = 2802
integer height = 1808
string title = "Búsqueda de Planificacion llegada"
boolean resizable = false
end type
global w_busc_planificallegada w_busc_planificallegada

type variables
DataWindowChild	idwc_planta, idwc_condicion
end variables

event open;Long		ll_fila = 1

istr_busq = Message.PowerObjectParm

istr_busq.argum[5]	= ""
istr_busq.argum[6]	= ""
istr_busq.argum[4]	= ""

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)

is_ordena = 'Packing:plan_copack,Fecha:plan_fechal,Hora:plan_horall'

TriggerEvent("ue_ordenamiento")

IF UpperBound(istr_busq.argum) > 0 THEN
	IF istr_busq.argum[2] <> "" THEN
		
		IF dw_1.Retrieve(Integer(istr_busq.argum[1]), Integer(istr_busq.argum[2])) > 0 THEN
					
			dw_1.SetFocus()
			dw_1.SelectRow(1,True)
		ELSE
			MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
			CloseWithReturn(This, istr_busq)
			Return
		END IF
	END IF
END IF
end event

on w_busc_planificallegada.create
call super::create
end on

on w_busc_planificallegada.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[3]	= String(dw_1.Object.plan_copack[dw_1.GetRow()])
istr_busq.argum[4]	= String(dw_1.Object.plan_fechal[dw_1.GetRow()])
istr_busq.argum[5]	= String(dw_1.Object.plan_horall[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_planificallegada
boolean visible = false
integer x = 2647
integer y = 1172
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_planificallegada
integer y = 760
integer width = 2322
integer taborder = 60
string dataobject = "dw_mues_busc_planificallegada"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_planificallegada
integer x = 2496
end type

type tab_1 from w_busqueda`tab_1 within w_busc_planificallegada
integer x = 82
integer width = 2322
integer height = 620
integer selectedtab = 2
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
boolean visible = false
integer width = 2286
integer height = 492
boolean enabled = false
string text = "Filtros                         "
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2217
integer y = 324
boolean enabled = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1]), Integer(istr_busq.argum[2])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)

ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)

END IF


end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2286
integer height = 492
string text = "Ordenamiento              "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2011
integer y = 288
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
boolean visible = false
integer width = 2286
integer height = 492
boolean enabled = false
string text = "Búsqueda                      "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
integer x = 549
integer y = 252
integer width = 411
boolean enabled = false
end type

event sle_argumento2::getfocus;call super::getfocus;This.SelectText(1, Len(This.Text))

This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(166,180,210)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "rfpe_fecrec"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
boolean visible = false
integer x = 133
integer y = 260
string text = "Fecha"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
boolean visible = false
integer x = 549
integer y = 132
integer width = 411
boolean enabled = false
end type

event sle_argumento1::getfocus;call super::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "rfpe_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
boolean visible = false
integer x = 133
integer y = 140
string text = "Folio"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2309
integer y = 312
end type

