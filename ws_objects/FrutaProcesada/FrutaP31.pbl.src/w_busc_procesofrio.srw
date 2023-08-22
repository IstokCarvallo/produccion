$PBExportHeader$w_busc_procesofrio.srw
forward
global type w_busc_procesofrio from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type uo_selplanta from uo_seleccion_plantas within tabpage_1
end type
end forward

global type w_busc_procesofrio from w_busqueda
integer x = 123
integer y = 304
integer width = 3182
string title = "Búsqueda de Pallets"
end type
global w_busc_procesofrio w_busc_procesofrio

type variables
Long	il_Proceso = -1

end variables

on w_busc_procesofrio.create
int iCurrent
call super::create
end on

on w_busc_procesofrio.destroy
call super::destroy
end on

event open;call super::open;Long		ll_fila = 1

istr_busq 				=	Message.PowerObjectParm

If UpperBound(istr_Busq.Argum) = 2 Then
	il_Proceso = Long(istr_Busq.Argum[2])
End If

dw_1.SetTransObject(sqlca)
dw_1.SetRowFocusIndicator(Hand!)

Tab_1.TabPage_1.uo_SelPlanta.Seleccion(False, False)
Tab_1.TabPage_1.uo_SelPlanta.Bloquear(True)
Tab_1.TabPage_1.uo_SelPlanta.Codigo = Long(istr_busq.Argum[1])
Tab_1.TabPage_1.uo_SelPlanta.dw_Seleccion.Object.Codigo[1] = Long(istr_busq.Argum[1])

is_ordena = 'Numero Pallet:paen_numero,Cliente:clie_codigo,Pallets:paen_tipopa'

TriggerEvent("ue_ordenamiento")

Tab_1.TabPage_1.pb_Filtrar.TriggerEvent(Clicked!)

IF dw_1.RowCount() > 0 THEN
	Tab_1.TabPage_1.uo_SelPlanta.Bloquear(True)
	dw_1.SetFocus()
ELSE
	CloseWithReturn(This, istr_busq)
	Return
END IF
end event

event ue_asignacion;istr_busq.argum[1]	= String(dw_1.GetItemDatetime(dw_1.GetRow(),"teen_fechas"), 'dd/mm/yyyy')
istr_busq.argum[2]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"teen_numero"))
istr_busq.argum[3]	= String(dw_1.GetItemNumber(dw_1.GetRow(),"cama_codigo"))


CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_procesofrio
boolean visible = false
integer x = 2834
integer y = 1108
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_procesofrio
integer x = 37
integer y = 732
integer width = 2697
integer taborder = 60
string dataobject = "dw_mues_nroprocesofrio"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_procesofrio
integer x = 2834
integer y = 1368
alignment htextalign = center!
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_procesofrio
integer x = 37
integer y = 92
integer width = 2766
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
integer width = 2729
string text = "Filtros              "
st_1 st_1
uo_selplanta uo_selplanta
end type

on tabpage_1.create
this.st_1=create st_1
this.uo_selplanta=create uo_selplanta
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selplanta
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selplanta)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2286
integer y = 296
integer taborder = 20
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;dw_1.Reset()

IF dw_1.Retrieve(uo_SelPlanta.Codigo, il_Proceso) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2729
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
boolean visible = false
integer width = 2729
boolean enabled = false
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
boolean visible = false
integer x = 571
integer y = 284
end type

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
boolean visible = false
integer x = 91
integer y = 280
integer width = 421
integer height = 88
string text = "Nombre"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 695
integer y = 136
integer width = 402
end type

event sle_argumento1::modified;is_argume	= This.Text
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 91
integer y = 148
integer width = 521
string text = "Número de Pallet"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 2459
integer y = 312
end type

event pb_buscar::clicked;Long		ll_fila

IF dw_1.RowCount() > 0 THEN
	dw_1.SetSort(is_busca + " A")
	dw_1.Sort()

	tab_1.tabpage_2.dw_3.Reset()
	
	IF es_numero THEN
		ll_fila	= dw_1.Find(is_busca + " >= " + is_argume, 1, dw_1.RowCount())
	ELSE
		is_busca	= "Mid(" + is_busca + ", 1, " + String(Len(is_argume)) + ")"
		ll_fila	= dw_1.Find(is_busca + " >= '" + is_argume + "'", 1, dw_1.RowCount())
	END IF

	IF ll_fila = 0 THEN ll_fila = 1
	
	dw_1.SetRedraw(False)
	dw_1.SetRow(ll_fila)
	dw_1.ScrollToRow(ll_fila)
	dw_1.SelectRow(0,False)
	dw_1.SelectRow(ll_fila,True)
	dw_1.SetFocus()
	dw_1.SetRedraw(True)
END IF
end event

type st_1 from statictext within tabpage_1
integer x = 512
integer y = 196
integer width = 197
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within tabpage_1
integer x = 731
integer y = 188
integer height = 88
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

