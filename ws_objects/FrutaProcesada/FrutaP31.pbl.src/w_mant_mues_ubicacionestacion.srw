$PBExportHeader$w_mant_mues_ubicacionestacion.srw
forward
global type w_mant_mues_ubicacionestacion from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_ubicacionestacion
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_ubicacionestacion
end type
end forward

global type w_mant_mues_ubicacionestacion from w_mant_tabla
integer width = 3113
string title = "Ubicación Estacion de Trabajo"
st_1 st_1
uo_selplanta uo_selplanta
end type
global w_mant_mues_ubicacionestacion w_mant_mues_ubicacionestacion

type variables
w_mant_deta_ubicacionestacion iw_mantencion

uo_grabatablas			iuo_grabatablas
end variables

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_mant.Argumento[1] = String(uo_SelPlanta.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE UBICACION ESTACIONES TRABAJO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_ubicacionestacion"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelPlanta.Codigo)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve(uo_SelPlanta.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_ubicacionestacion.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selplanta
end on

on w_mant_mues_ubicacionestacion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selplanta)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
	
	IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Codigo = gi_codplanta
	uo_SelPlanta.dw_Seleccion.object.codigo[1] = gi_codplanta

	buscar	= "Código:Nubic_codigo,Descripción:Subic_nombre"
	ordenar	= "Código:ubic_codigo,Nombre:ubic_nombre"
End If

end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_ubicacionestacion
integer x = 114
integer y = 344
integer width = 2322
integer height = 1400
string dataobject = "dw_mues_ubicacionestacion"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_ubicacionestacion
integer x = 133
integer y = 20
integer width = 2322
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_ubicacionestacion
integer x = 2683
integer y = 112
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_ubicacionestacion
boolean visible = false
integer x = 3881
integer y = 392
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_ubicacionestacion
integer x = 2679
integer y = 648
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_ubicacionestacion
integer x = 2679
integer y = 392
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_ubicacionestacion
integer x = 2679
integer y = 904
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_ubicacionestacion
integer x = 2697
integer y = 1188
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_ubicacionestacion
integer x = 2683
integer y = 1520
end type

type st_1 from statictext within w_mant_mues_ubicacionestacion
integer x = 398
integer y = 116
integer width = 261
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_ubicacionestacion
integer x = 709
integer y = 104
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

