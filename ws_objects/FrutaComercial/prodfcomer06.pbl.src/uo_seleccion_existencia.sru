$PBExportHeader$uo_seleccion_existencia.sru
forward
global type uo_seleccion_existencia from userobject
end type
type cb_4 from commandbutton within uo_seleccion_existencia
end type
type cb_3 from commandbutton within uo_seleccion_existencia
end type
type cb_2 from commandbutton within uo_seleccion_existencia
end type
type cb_1 from commandbutton within uo_seleccion_existencia
end type
type dw_2 from uo_dw within uo_seleccion_existencia
end type
type dw_1 from uo_dw within uo_seleccion_existencia
end type
end forward

global type uo_seleccion_existencia from userobject
integer width = 3621
integer height = 1560
long backcolor = 12632256
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_4 cb_4
cb_3 cb_3
cb_2 cb_2
cb_1 cb_1
dw_2 dw_2
dw_1 dw_1
end type
global uo_seleccion_existencia uo_seleccion_existencia

type variables
DataWindow	dw
Long				Planta
end variables

forward prototypes
public function boolean inicializa (integer ai_planta, datawindow ad_dw, string ls_objeto)
public function boolean recupera ()
public function boolean carga ()
public function boolean filtra (datawindow adw, string columna, integer estado)
public function boolean filtra (datawindow adw)
public function boolean marca ()
end prototypes

public function boolean inicializa (integer ai_planta, datawindow ad_dw, string ls_objeto);Boolean	lb_Retorno = True

Planta		=	ai_Planta
dw			=	ad_dw

dw_1.DataObject	=	ls_objeto
dw_2.DataObject	=	ls_objeto

If dw_1.SetTransObject(sqlca) = -1 Then lb_Retorno = False
If dw_2.SetTransObject(sqlca) = -1 Then lb_Retorno = False

Return lb_Retorno
end function

public function boolean recupera ();Boolean	lb_Retorno = True

dw_1.SetRedraw(False)
dw_2.SetRedraw(False)

If dw_1.Retrieve(Planta) = -1 Then 
	lb_Retorno = False
Else
	Marca()
End If

dw_1.SetRedraw(True)
dw_2.SetRedraw(True)

Return lb_Retorno
end function

public function boolean carga ();Boolean	lb_Retorno = True
Long		ll_Fila, ll_New

If dw_2.RowCount() > 0 Then
	For ll_Fila = 1 To dw_2.RowCount()
		ll_New	= dw.InsertRow(0)
		
		dw.Object.plde_codigo[ll_New] 	= dw_2.Object.plde_codigo[ll_Fila]
		dw.Object.tpmv_codigo[ll_New] 	= 33
		dw.Object.cama_codigo[ll_New] 	= dw_2.Object.cama_codigo[ll_Fila]
		dw.Object.lofc_pltcod[ll_New] 		= dw_2.Object.lofc_pltcod[ll_Fila]
		dw.Object.lofc_espcod[ll_New] 	= dw_2.Object.lofc_espcod[ll_Fila]
		dw.Object.lofc_lotefc[ll_New] 		= dw_2.Object.lofc_lotefc[ll_Fila]
		dw.Object.lfcd_secuen[ll_New] 	= dw_2.Object.lfcd_secuen[ll_Fila]
		dw.Object.mfcd_bulent[ll_New] 	= dw_2.Object.caex_canbul[ll_Fila]
		dw.Object.mfcd_kgnent[ll_New] 	= dw_2.Object.lfcd_kilnet[ll_Fila]
		dw.Object.mfcd_kilrom[ll_New] 	= dw_2.Object.lfcd_kilbru[ll_Fila]
		dw.Object.vari_nombre[ll_New] 	= dw_2.Object.vari_nombre[ll_Fila]
		dw.Object.prod_nombre[ll_New] 	= dw_2.Object.prod_nombre[ll_Fila]
		dw.Object.clie_codigo[ll_New] 		= gi_CodExport
		dw.Object.cale_calida[ll_New] 		= dw_2.Object.cale_calida[ll_Fila]
		dw.Object.prod_codigo[ll_New] 	= dw_2.Object.prod_codigo[ll_Fila]
		dw.Object.enva_tipoen[ll_New] 	= dw_2.Object.enva_tipoen[ll_Fila]
		dw.Object.enva_codigo[ll_New] 	= dw_2.Object.enva_codigo[ll_Fila]
		dw.Object.plde_coorde[ll_New] 	= dw_2.Object.plde_codigo[ll_Fila]
	Next
Else
	lb_Retorno = False
End If

Return lb_Retorno
end function

public function boolean filtra (datawindow adw, string columna, integer estado);Boolean	lb_Retorno = True

adw.SetRedraw(False)
adw.SetFilter('')
adw.Filter()

adw.SetFilter(columna + ' = ' + String(Estado))
If adw.Filter() = -1 Then
	lb_Retorno = False
End If

adw.SetRedraw(True)

Return lb_Retorno
end function

public function boolean filtra (datawindow adw);Boolean	lb_Retorno = True

adw.SetRedraw(False)
adw.SetFilter('')
adw.Filter()

adw.SetFilter('')
If adw.Filter() = -1 Then
	lb_Retorno = False
End If

adw.SetRedraw(True)

Return lb_Retorno
end function

public function boolean marca ();Boolean	lb_Retorno = True
Long		ll_Fila, ll_Busca
String		ls_Busca

If dw.RowCount() = 0 Then
	lb_Retorno = False
Else
	For ll_Fila = 1 To dw.RowCount()
		ls_Busca		=	'cama_codigo = ' + String(dw.Object.cama_codigo[ll_Fila]) + ' And ' + &
							'lofc_pltcod = ' + String(dw.Object.lofc_pltcod[ll_Fila]) + ' And ' + &
							'lofc_espcod = ' + String(dw.Object.lofc_espcod[ll_Fila]) + ' And ' + &
							'lofc_lotefc = ' + String(dw.Object.lofc_lotefc[ll_Fila]) + ' And ' + &
							'lfcd_secuen = ' + String(dw.Object.lfcd_secuen[ll_Fila])
		
		ll_Busca = dw_1.Find(ls_Busca, 1, dw_1.RowCount())
		
		If ll_Busca > 0 Then
			dw_1.Object.estado[ll_Busca] = 1
		ElseIf ll_Busca < 0 Then
			lb_Retorno = False
		End If		
	Next
	
	If Not Filtra(dw_1, 'estado', 0) Then lb_Retorno = False
End If

Return lb_Retorno
end function

on uo_seleccion_existencia.create
this.cb_4=create cb_4
this.cb_3=create cb_3
this.cb_2=create cb_2
this.cb_1=create cb_1
this.dw_2=create dw_2
this.dw_1=create dw_1
this.Control[]={this.cb_4,&
this.cb_3,&
this.cb_2,&
this.cb_1,&
this.dw_2,&
this.dw_1}
end on

on uo_seleccion_existencia.destroy
destroy(this.cb_4)
destroy(this.cb_3)
destroy(this.cb_2)
destroy(this.cb_1)
destroy(this.dw_2)
destroy(this.dw_1)
end on

type cb_4 from commandbutton within uo_seleccion_existencia
integer x = 27
integer y = 1180
integer width = 219
integer height = 72
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "< <"
end type

event clicked;dw_1.SetRedraw(False)
dw_2.SetRedraw(False)

dw_2.RowsMove(1, dw_2.RowCount(),  Primary!, dw_1, 1, Primary!)

dw_1.SetRedraw(True)
dw_2.SetRedraw(True)
end event

type cb_3 from commandbutton within uo_seleccion_existencia
integer x = 27
integer y = 1080
integer width = 219
integer height = 72
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<"
end type

event clicked;Long	ll_Fila

dw_1.SetRedraw(False)
dw_2.SetRedraw(False)

For ll_fila = 1 To dw_2.RowCount()
	If dw_2.IsSelected(ll_Fila) Then
		dw_2.RowsMove(ll_Fila, ll_Fila, Primary!, dw_1, 1, Primary!)
		ll_Fila = 0
	End If
Next

dw_1.SetRedraw(True)
dw_2.SetRedraw(True)
end event

type cb_2 from commandbutton within uo_seleccion_existencia
integer x = 27
integer y = 408
integer width = 219
integer height = 72
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "> >"
end type

event clicked;dw_1.SetRedraw(False)
dw_2.SetRedraw(False)

dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_2, 1, Primary!)

dw_1.SetRedraw(True)
dw_2.SetRedraw(True)
end event

type cb_1 from commandbutton within uo_seleccion_existencia
integer x = 27
integer y = 312
integer width = 219
integer height = 72
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = ">"
end type

event clicked;Long	ll_Fila

dw_1.SetRedraw(False)
dw_2.SetRedraw(False)

For ll_fila = 1 To dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		dw_1.RowsMove(ll_Fila, ll_Fila, Primary!, dw_2, 1, Primary!)
		ll_Fila = 0
	End If
Next

dw_1.SetRedraw(True)
dw_2.SetRedraw(True)
end event

type dw_2 from uo_dw within uo_seleccion_existencia
integer x = 256
integer y = 796
integer width = 3351
integer height = 744
integer taborder = 20
string dataobject = "dw_consulta_existencia"
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;String		ls_Tecla

If KeyDown(KeyShift!) Then
	ls_tecla	=	"Shift"
ElseIf KeyDown(KeyControl!) Then
	ls_tecla	=	"Control"
End If

F_Selecciona(This, ls_Tecla, Row)
end event

event doubleclicked;call super::doubleclicked;This.SetRedraw(False)
dw_1.SetRedraw(False)

If Row <= 0 Then Return

This.RowsMove(Row, Row, Primary!, dw_1, 1, Primary!)

This.SetRedraw(True)
dw_1.SetRedraw(True)
end event

type dw_1 from uo_dw within uo_seleccion_existencia
integer x = 256
integer y = 28
integer width = 3351
integer height = 744
integer taborder = 10
string dataobject = "dw_consulta_existencia"
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;String		ls_Tecla

If KeyDown(KeyShift!) Then
	ls_tecla	=	"Shift"
ElseIf KeyDown(KeyControl!) Then
	ls_tecla	=	"Control"
End If

F_Selecciona(This, ls_Tecla, Row)



end event

event doubleclicked;call super::doubleclicked;This.SetRedraw(False)
dw_2.SetRedraw(False)

If Row <= 0 Then Return

This.RowsMove(Row, Row, Primary!, dw_2, 1, Primary!)

This.SetRedraw(True)
dw_2.SetRedraw(True)
end event

