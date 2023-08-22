$PBExportHeader$w_seleccion_tarjas.srw
forward
global type w_seleccion_tarjas from w_mant_detalle
end type
type st_1 from statictext within w_seleccion_tarjas
end type
type st_2 from statictext within w_seleccion_tarjas
end type
type cb_aplicar from commandbutton within w_seleccion_tarjas
end type
type cb_aplicatodo from commandbutton within w_seleccion_tarjas
end type
type cb_desaplica from commandbutton within w_seleccion_tarjas
end type
type cb_desaplicatodo from commandbutton within w_seleccion_tarjas
end type
type dw_2 from datawindow within w_seleccion_tarjas
end type
end forward

global type w_seleccion_tarjas from w_mant_detalle
integer width = 3109
integer height = 1628
st_1 st_1
st_2 st_2
cb_aplicar cb_aplicar
cb_aplicatodo cb_aplicatodo
cb_desaplica cb_desaplica
cb_desaplicatodo cb_desaplicatodo
dw_2 dw_2
end type
global w_seleccion_tarjas w_seleccion_tarjas

on w_seleccion_tarjas.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.cb_aplicar=create cb_aplicar
this.cb_aplicatodo=create cb_aplicatodo
this.cb_desaplica=create cb_desaplica
this.cb_desaplicatodo=create cb_desaplicatodo
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.cb_aplicar
this.Control[iCurrent+4]=this.cb_aplicatodo
this.Control[iCurrent+5]=this.cb_desaplica
this.Control[iCurrent+6]=this.cb_desaplicatodo
this.Control[iCurrent+7]=this.dw_2
end on

on w_seleccion_tarjas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_aplicar)
destroy(this.cb_aplicatodo)
destroy(this.cb_desaplica)
destroy(this.cb_desaplicatodo)
destroy(this.dw_2)
end on

event ue_borrar;call super::ue_borrar;Long	ll_fila1

setpointer(hourglass!)

ib_borrar 	=	TRUE

IF dw_1.rowcount() < 1 THEN RETURN

IF messagebox("Borrar registro(s)","¿Desea Eliminar la fila seleccionada?",&
				exclamation!,yesno!,2) <> 1 THEN
	RETURN
END IF

w_main.setmicrohelp("Validando la Eliminación...")

ll_fila1	=	dw_1.GetRow()

IF dw_1.RowsCopy(ll_fila1, ll_fila1, Primary!, dw_2, dw_2.RowCount() + 1, Primary!) > 0 THEN
	dw_2.Sort()
	dw_1.DeleteRow(ll_fila1)
	w_main.setmicrohelp("Registro Borrado.")
	setpointer(Arrow!)
	
ELSE
	ib_borrar = FALSE
	messagebox(THIS.title, "No se puede borrar registro actual.")
	
END IF

end event

event open;call super::open;Integer	li_filas, li_respuesta, li_find
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

dw_2.SetTransObject(SQLCA)

DO
	li_filas	=	dw_2.Retrieve(Long(istr_mant.Argumento[1]), &
									  Integer(istr_mant.Argumento[2]), &
									  Long(istr_mant.Argumento[3]),&
									  Integer(istr_mant.Argumento[4]),&
									  Integer(istr_mant.Argumento[5]))
									  
									  
	IF li_filas = -1 THEN
		li_respuesta = MessageBox("Error", "No se pudo conectar con la base de datos.~r~n"	+	&
													  "¿Desea Reintentar?", Question!, YesNo!, 1)
	ELSEIF li_filas = 0 THEN
		MessageBox("Error", "No existen tarjas en existencia para el lote indicado,~r~n" 	+	&
								  "Por favor ingrese o seleccione otro.", StopSign!)
		Close(This)
		
	END IF

LOOP WHILE li_respuesta = 2


FOR li_filas = 1 TO dw_1.RowCount()
	li_find	=	dw_2.Find("fgmb_nrotar = " + String(dw_1.Object.fgmb_nrotar[li_filas]), &
								 1, dw_1.RowCount())
								 
	IF li_find > 0 THEN
		dw_2.DeleteRow(li_find)
	END IF
	
NEXT
end event

event resize;//
end event

event ue_recuperadatos;w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

IF istr_mant.Agrega THEN
	pb_Cancela.Enabled	=	False
	pb_Primero.Enabled	=	False
	pb_Anterior.Enabled	=	False
	pb_Siguiente.Enabled	=	False
	pb_Ultimo.Enabled		=	False
	
//	wf_nuevo()
	This.Title			= "INGRESO DE REGISTRO"
ELSE
	IF dw_1.RowCount() > 1 THEN
		pb_Primero.Enabled	=	True
		pb_Anterior.Enabled	=	True
		pb_Siguiente.Enabled	=	True
		pb_Ultimo.Enabled		=	True
	END IF
	
	il_fila	=	istr_mant.dw.GetRow()
	
	dw_1.SetRedraw(False)
	dw_1.SetRow(il_fila)
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRedraw(True)

	IF istr_mant.Borra THEN
		dw_1.Enabled		=	False
		pb_Salir.Enabled	=	False
		This.Title			=	"ELIMINACION DE REGISTRO"
		
	ELSEIF istr_mant.Solo_Consulta THEN
		dw_1.Enabled			=	False
		pb_Acepta.Enabled		=	False
		pb_Cancela.Enabled	=	False
		This.Title				=	"CONSULTA DE REGISTRO"
		
	ELSE
		pb_Salir.Enabled	=	False
		This.Title			=	"MANTENCION DE REGISTRO"
		
	END IF
END IF
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_seleccion_tarjas
boolean visible = false
integer x = 3269
integer y = 872
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_seleccion_tarjas
boolean visible = false
integer x = 3269
integer y = 872
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_seleccion_tarjas
boolean visible = false
integer x = 3269
integer y = 872
end type

type pb_primero from w_mant_detalle`pb_primero within w_seleccion_tarjas
boolean visible = false
integer x = 3269
integer y = 872
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_seleccion_tarjas
integer x = 2725
integer y = 352
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_seleccion_tarjas
integer x = 2725
integer y = 96
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

If istr_mant.Agrega Then
//	ComparteDatos(il_Fila, il_FilaAnc, dw_1, istr_mant.Dw)
	Parent.TriggerEvent("ue_nuevo")
Else
	If Not istr_mant.Borra Then
	//	ComparteDatos(il_Fila, il_FilaAnc, dw_1, istr_mant.Dw)
	End If
	
	CloseWithReturn(Parent, istr_mant)
End If
end event

type pb_salir from w_mant_detalle`pb_salir within w_seleccion_tarjas
integer x = 2725
integer y = 608
end type

type dw_1 from w_mant_detalle`dw_1 within w_seleccion_tarjas
integer x = 576
integer y = 712
integer width = 2007
boolean titlebar = true
string title = "Tarjas Seleccionadas en Despacho Lote"
string dataobject = "dw_mues_spro_movtograndeta_tarjas"
boolean vscrollbar = true
boolean livescroll = true
end type

event dw_1::clicked;call super::clicked;IF row > 0 THEN
	This.SelectRow(0, False)
	This.SelectRow(Row, True)
	This.SetRow(Row)
END IF
end event

type st_1 from statictext within w_seleccion_tarjas
integer x = 46
integer y = 32
integer width = 526
integer height = 680
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
long bordercolor = 16777215
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_seleccion_tarjas
integer x = 46
integer y = 712
integer width = 526
integer height = 816
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_aplicar from commandbutton within w_seleccion_tarjas
integer x = 101
integer y = 212
integer width = 430
integer height = 112
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Aplica"
end type

event clicked;Long		ll_fila1, ll_fila2
Integer	li_causal

ll_fila2 = dw_2.GetSelectedRow(0)

IF ll_fila2 = 0 THEN
	MessageBox("Error de Consistencia","Debe seleccionar Lotes Clasificados previamente ")
	RETURN
END IF

SetPointer(HourGlass!)

DO WHILE ll_fila2 > 0
	ll_fila1	=	dw_1.RowCount() + 1

	dw_2.RowsMove(ll_fila2, ll_fila2, Primary! ,dw_1, ll_fila1, Primary!)

	cb_desaplica.Enabled			= True
	cb_desaplicatodo.Enabled	= True

	ll_fila2 = dw_2.GetSelectedRow(0)
	
LOOP

dw_1.SetFocus()
end event

type cb_aplicatodo from commandbutton within w_seleccion_tarjas
integer x = 101
integer y = 396
integer width = 430
integer height = 112
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Aplica Todos"
end type

event clicked;Long		ll_fila2,ll_fila1
Boolean	lb_respuesta = True

ll_fila2 = dw_2.Rowcount()

IF ll_fila2 = 0 THEN
	MessageBox("Error de Consistencia","No Existe Lotes Clasificados")
	RETURN 
END IF

SetPointer(HourGlass!)

IF lb_respuesta THEN
	ll_fila1	=	dw_1.RowCount() + 1
	dw_2.RowsMove(1,ll_fila2,Primary!,dw_1,ll_fila1,Primary!)
	
	cb_desaplica.Enabled			= True
	cb_desaplicatodo.Enabled	= True

	dw_1.SetFocus()
	
ELSE
	dw_2.SelectRow(ll_fila2,False)
	dw_2.SetFocus()
	
END IF
end event

type cb_desaplica from commandbutton within w_seleccion_tarjas
integer x = 101
integer y = 912
integer width = 430
integer height = 112
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Desaplica"
end type

event clicked;PARENT.TriggerEvent("ue_borrar")
end event

type cb_desaplicatodo from commandbutton within w_seleccion_tarjas
integer x = 101
integer y = 1096
integer width = 430
integer height = 112
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Des. Todos"
end type

event clicked;Long		ll_filas

IF dw_1.rowcount() < 1 THEN RETURN

IF messagebox("Borrar registros","¿Desea Eliminar TODAS LAS FILAS asignadas?", exclamation!, yesno!, 2) <> 1 THEN
	RETURN

END IF

SetPointer(hourglass!)

ib_borrar 				= 	TRUE

w_main.setmicrohelp("Validando la eliminación...")

message.doubleparm 	= 	0

PARENT.triggerevent ("ue_validaborrar")

IF message.doubleparm = -1 THEN RETURN

dw_1.RowsCopy(1, dw_1.RowCount(), Primary!, dw_2, dw_2.RowCount() + 1, Primary!)
dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)

cb_aplicar.Enabled		= 	TRUE
cb_aplicatodo.Enabled	= 	TRUE

dw_2.SetFocus()
end event

type dw_2 from datawindow within w_seleccion_tarjas
integer x = 576
integer y = 32
integer width = 2007
integer height = 680
integer taborder = 50
boolean titlebar = true
string title = "Tarjas Disponibles del Lote Seleccionado"
string dataobject = "dw_mues_spro_movtograndeta_lotes_disp"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF row > 0 THEN
	IF IsSelected(row) THEN
		SelectRow(row,False)
		
	ELSE
		SelectRow(row,True)
		
	END IF
END IF
end event

