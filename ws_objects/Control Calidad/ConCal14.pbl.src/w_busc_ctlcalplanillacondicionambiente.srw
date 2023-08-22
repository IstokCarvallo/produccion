$PBExportHeader$w_busc_ctlcalplanillacondicionambiente.srw
forward
global type w_busc_ctlcalplanillacondicionambiente from w_busqueda
end type
end forward

global type w_busc_ctlcalplanillacondicionambiente from w_busqueda
integer x = 78
integer y = 176
integer width = 3008
integer height = 1832
string title = "Búsqueda de Planillas Parámetros de Madurez"
boolean resizable = false
end type
global w_busc_ctlcalplanillacondicionambiente w_busc_ctlcalplanillacondicionambiente

type variables
DataWindowChild	idwc_variedades

Integer	ii_fila
Integer  ii_SwOrden[]	=	{0, 0, 0}
end variables

event open;istr_busq	= Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

is_ordena = 'Planilla:coam_numero,Zona:zona_codigo,Productor:prod_codigo,Variedad:vari_codigo'

IF dw_1.Retrieve(Integer(istr_busq.Argum[4]), Integer(istr_busq.Argum[1]), Integer(istr_busq.Argum[2])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	CloseWithReturn(This,istr_busq)
END IF

end event

on w_busc_ctlcalplanillacondicionambiente.create
call super::create
end on

on w_busc_ctlcalplanillacondicionambiente.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[1]	= String(dw_1.GetItemNumber(dw_1.GetRow(), "plde_codigo"))
istr_busq.argum[2]	= String(dw_1.GetItemNumber(dw_1.GetRow(), "espe_codigo"))
istr_busq.argum[4]	= String(dw_1.GetItemNumber(dw_1.GetRow(), "clie_codigo"))
istr_busq.argum[5]	= String(dw_1.GetItemNumber(dw_1.GetRow(), "coam_numero"))

CloseWithReturn(This, istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_ctlcalplanillacondicionambiente
boolean visible = false
integer x = 2665
integer y = 1128
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_ctlcalplanillacondicionambiente
integer y = 760
integer width = 2551
integer height = 900
integer taborder = 60
string dataobject = "dw_mues_ctlcalplanillacondicionambiente"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_ctlcalplanillacondicionambiente
integer x = 2747
integer y = 1412
end type

event pb_salir::clicked;istr_busq.argum[3]	=	""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_ctlcalplanillacondicionambiente
integer x = 73
integer y = 96
integer width = 2555
integer height = 628
integer selectedtab = 2
end type

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
boolean visible = false
integer width = 2519
integer height = 500
boolean enabled = false
string text = "Filtros                         "
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2304
integer y = 184
integer weight = 400
fontcharset fontcharset = ansi!
end type

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2519
integer height = 500
string text = "Ordenamiento              "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2304
integer taborder = 20
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer taborder = 30
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer taborder = 10
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
boolean visible = false
integer width = 2519
integer height = 500
boolean enabled = false
string text = "Búsqueda                      "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 704
integer y = 252
integer width = 411
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))

This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "cclo_numero"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 133
integer y = 260
string text = "Nº. Planilla"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 704
integer y = 132
integer width = 411
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "prod_codigo"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 133
integer y = 140
integer width = 521
string text = "Productor"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2309
integer y = 312
end type

