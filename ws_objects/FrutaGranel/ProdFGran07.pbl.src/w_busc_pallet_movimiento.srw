$PBExportHeader$w_busc_pallet_movimiento.srw
$PBExportComments$busqueda de pallet por movimiento
forward
global type w_busc_pallet_movimiento from w_busqueda
end type
end forward

global type w_busc_pallet_movimiento from w_busqueda
integer x = 123
integer y = 304
integer width = 3534
integer height = 1888
string title = "Búsqueda de Pallet"
end type
global w_busc_pallet_movimiento w_busc_pallet_movimiento

type variables

end variables

on w_busc_pallet_movimiento.create
call super::create
end on

on w_busc_pallet_movimiento.destroy
call super::destroy
end on

event open;DataWindowChild		dwc1, ldwc_especie

istr_busq	= Message.PowerObjectParm

Call SUPER::Open

dw_1.GetChild("espe_codigo", ldwc_especie)
ldwc_especie.SetTransObject(SqlCa)
IF ldwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Especie")
	ldwc_especie.InsertRow(0)
END IF

is_ordena	=	'Nro Pallet:paen_numero,' + &
					'Variedad:vari_nombre,Cajas:paen_ccajas, Nro. Orden:mfee_docrel'

dw_1.SetReDraw(False)

IF dw_1.Retrieve(long(istr_busq.Argum[1]),&
						Integer(istr_busq.Argum[2]), &
                 			long(istr_busq.Argum[3]),&
						integer(istr_busq.Argum[5])) > 0 THEN
	
	IF istr_busq.argum[4] <> "0" THEN
		dw_1.SetFilter("espe_codigo = " + istr_busq.argum[4] + " AND " + &
							"clie_codigo = " + istr_busq.argum[5] + " AND " + &
							"paen_estado = 1")
	ELSE
		dw_1.SetFilter("clie_codigo = " + istr_busq.argum[5] + " AND " + &
							"paen_estado = 1")
	END IF
	dw_1.Filter()
	dw_1.SetReDraw(TRUE)				  
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
	istr_busq.Argum[1] = ""
	istr_busq.Argum[2] = ""
	istr_busq.Argum[3] = ""
	CloseWithReturn(This,istr_busq)
END IF
end event

event ue_asignacion;istr_busq.argum[1]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"clie_codigo"))
istr_busq.argum[2]	= String(dw_1.Object.paen_numero[dw_1.GetRow()])
istr_busq.argum[3]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"espe_codigo"))
istr_busq.argum[4]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"vari_codigo"))

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_pallet_movimiento
boolean visible = false
integer x = 3127
integer y = 1116
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_pallet_movimiento
integer x = 91
integer y = 748
integer width = 2917
integer height = 992
integer taborder = 60
string dataobject = "dw_mant_mues_palletencab_eliminacion"
boolean hsplitscroll = true
end type

type pb_salir from w_busqueda`pb_salir within w_busc_pallet_movimiento
integer x = 3136
integer y = 1376
end type

event pb_salir::clicked;istr_busq.Argum[1] = ""
istr_busq.Argum[2] = ""
istr_busq.Argum[3] = ""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_pallet_movimiento
integer x = 87
integer y = 64
integer width = 2277
boolean fixedwidth = true
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
integer width = 2240
string text = "Filtros                                   "
end type

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2747
integer y = 308
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
integer width = 2240
string text = "Ordenamiento    "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1865
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer y = 40
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer y = 40
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2240
string text = "Búsqueda    "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 439
integer y = 244
integer width = 1243
end type

event sle_argumento2::getfocus;call super::getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 78
integer y = 252
integer width = 325
string text = "Variedad"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 439
integer y = 124
integer width = 210
end type

event sle_argumento1::getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "paen_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 73
integer y = 132
integer width = 302
string text = "Nro Pallet"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1874
integer y = 304
end type

