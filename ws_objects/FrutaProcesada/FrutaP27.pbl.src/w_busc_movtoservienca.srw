$PBExportHeader$w_busc_movtoservienca.srw
forward
global type w_busc_movtoservienca from w_busqueda
end type
end forward

global type w_busc_movtoservienca from w_busqueda
integer x = 78
integer y = 176
integer width = 3278
integer height = 1752
string title = "Búsqueda de Movimiento Servicios"
boolean resizable = false
end type
global w_busc_movtoservienca w_busc_movtoservienca

type variables
DataWindowChild	idwc_planta, idwc_condicion
end variables

event open;Long		ll_fila = 1

istr_busq = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.SetRowFocusIndicator(Hand!)
	
IF dw_1.Retrieve(Integer(istr_busq.argum[2]), Integer(istr_busq.argum[1])) > 0 THEN
	
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	CloseWithReturn(This, istr_busq)
	Return
END IF

end event

on w_busc_movtoservienca.create
call super::create
end on

on w_busc_movtoservienca.destroy
call super::destroy
end on

event ue_asignacion;istr_busq.argum[3]	= String(dw_1.Object.mose_numero[dw_1.GetRow()])

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_movtoservienca
boolean visible = false
integer x = 3054
integer y = 1164
integer taborder = 50
boolean enabled = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_movtoservienca
integer y = 760
integer width = 2702
integer taborder = 60
string dataobject = "dw_mues_movtoservienca"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_movtoservienca
integer x = 2903
integer y = 1388
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_movtoservienca
integer x = 82
integer y = 64
integer width = 2702
integer height = 620
boolean enabled = false
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
integer x = 18
integer y = 112
integer width = 2665
integer height = 492
boolean enabled = false
string text = "Filtros                         "
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
boolean visible = false
integer x = 2030
integer y = 328
end type

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer x = 18
integer y = 112
integer width = 2665
integer height = 492
boolean enabled = false
string text = "Ordenamiento              "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 2043
integer y = 292
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer x = 18
integer y = 112
integer width = 2665
integer height = 492
boolean enabled = false
string text = "Búsqueda                      "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 544
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
is_busca							= "fumi_fecfum"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 133
integer y = 260
string text = "Fecha"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 544
integer y = 132
integer width = 411
end type

event sle_argumento1::getfocus;call super::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(166,180,210)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "fumi_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 133
integer y = 140
string text = "Proceso"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
boolean visible = false
integer x = 1125
integer y = 112
boolean enabled = false
end type

