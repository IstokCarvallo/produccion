$PBExportHeader$w_mant_mues_trazabilidadestacion.srw
forward
global type w_mant_mues_trazabilidadestacion from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_trazabilidadestacion
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_trazabilidadestacion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_trazabilidadestacion
end type
type uo_selestacion from uo_seleccion_estaciontrabajo within w_mant_mues_trazabilidadestacion
end type
type st_2 from statictext within w_mant_mues_trazabilidadestacion
end type
type st_3 from statictext within w_mant_mues_trazabilidadestacion
end type
type st_4 from statictext within w_mant_mues_trazabilidadestacion
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_trazabilidadestacion
end type
end forward

global type w_mant_mues_trazabilidadestacion from w_mant_tabla
integer width = 3781
string title = "Trazabilidad Estacion de Trabajo"
st_1 st_1
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
uo_selestacion uo_selestacion
st_2 st_2
st_3 st_3
st_4 st_4
uo_selespecie uo_selespecie
end type
global w_mant_mues_trazabilidadestacion w_mant_mues_trazabilidadestacion

type variables
w_mant_deta_plantillatrazabilidad iw_mantencion

DataWindowChild	idwc_variedad

uo_grabatablas			iuo_grabatablas


end variables

event ue_nuevo;istr_mant.borra		= False
istr_mant.agrega	= True

istr_mant.Argumento[1] = String(uo_SelCliente.Codigo)
istr_mant.Argumento[2] = String(uo_SelPlanta.Codigo)
istr_mant.Argumento[3] = String(uo_SelEstacion.Codigo)
istr_mant.Argumento[4] = String(uo_SelEspecie.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

If dw_1.RowCount() > 0 And Not pb_eliminar.Enabled Then
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled	= True
End If

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE PLANTILLAS TRAZABILIDAD"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_plantillastrazabilidad"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEstacion.Codigo, uo_SelEspecie.Codigo)

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
	
	uo_SelPlanta.Bloquear(True)
	uo_SelCliente.Bloquear(True)
	uo_SelEstacion.Bloquear(True)
	uo_SelEspecie.Bloquear(True)

	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEstacion.Codigo, uo_SelEspecie.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
		pb_nuevo.Enabled	= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_trazabilidadestacion.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.uo_selestacion=create uo_selestacion
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selplanta
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selestacion
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.uo_selespecie
end on

on w_mant_mues_trazabilidadestacion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.uo_selestacion)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.uo_selespecie)
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

istr_mant.Argumento[1] = String(uo_SelCliente.Codigo)
istr_mant.Argumento[2] = String(uo_SelPlanta.Codigo)
istr_mant.Argumento[3] = String(uo_SelEstacion.Codigo)
istr_mant.Argumento[4] = String(uo_SelEspecie.Codigo)

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
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEstacion.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Codigo = gi_codplanta
	uo_SelPlanta.dw_Seleccion.object.codigo[1] = gi_codplanta
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Codigo = gi_codExport
	uo_SelCliente.dw_Seleccion.object.codigo[1] = gi_codExport
	uo_SelEspecie.Seleccion(False, False)
	uo_SelEspecie.Codigo = gi_CodEspecie
	uo_SelEspecie.dw_Seleccion.object.codigo[1] = gi_CodEspecie
	uo_SelEstacion.Seleccion(False, False)
	
	
	dw_1.GetChild("vari_codigo", idwc_variedad)
	idwc_variedad.SetTransObject(Sqlca)
	idwc_variedad.Retrieve(gi_CodEspecie)


	buscar	= "Actividad:Nacti_codigo,Ubicacion:Nubic_codigo,Variedad:Nvari_codigo"
	ordenar	= "Actividad:acti_codigo,Ubicacion:ubic_codigo,Variedad:vari_codigo"
End If

end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	
	istr_mant.Argumento[1] = String(uo_SelCliente.Codigo)
	istr_mant.Argumento[2] = String(uo_SelPlanta.Codigo)
	istr_mant.Argumento[3] = String(uo_SelEstacion.Codigo)
	istr_mant.Argumento[4] = String(uo_SelEspecie.Codigo)

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_trazabilidadestacion
integer x = 133
integer y = 480
integer width = 3081
integer height = 1224
string dataobject = "dw_mues_plantillastrazabilidad"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_trazabilidadestacion
integer x = 133
integer y = 20
integer width = 3058
integer height = 424
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_trazabilidadestacion
integer x = 3429
integer y = 116
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_trazabilidadestacion
integer x = 3419
integer y = 384
boolean enabled = false
end type

event pb_nuevo::clicked;call super::clicked;	uo_SelPlanta.Bloquear(False)
	uo_SelCliente.Bloquear(False)
	uo_SelEstacion.Bloquear(False)
	uo_SelEspecie.Bloquear(False)
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_trazabilidadestacion
integer x = 3424
integer y = 748
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_trazabilidadestacion
integer x = 3415
integer y = 548
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_trazabilidadestacion
integer x = 3424
integer y = 980
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_trazabilidadestacion
integer x = 3424
integer y = 1196
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_trazabilidadestacion
integer x = 3429
integer y = 1524
end type

type st_1 from statictext within w_mant_mues_trazabilidadestacion
integer x = 192
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
string text = "Cliente"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_trazabilidadestacion
integer x = 503
integer y = 272
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_trazabilidadestacion
integer x = 503
integer y = 104
integer height = 88
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selestacion from uo_seleccion_estaciontrabajo within w_mant_mues_trazabilidadestacion
integer x = 2066
integer y = 104
integer height = 88
integer taborder = 30
boolean bringtotop = true
end type

on uo_selestacion.destroy
call uo_seleccion_estaciontrabajo::destroy
end on

type st_2 from statictext within w_mant_mues_trazabilidadestacion
integer x = 192
integer y = 284
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

type st_3 from statictext within w_mant_mues_trazabilidadestacion
integer x = 1755
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
string text = "Estacion"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_trazabilidadestacion
integer x = 1755
integer y = 284
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_trazabilidadestacion
integer x = 2066
integer y = 272
integer height = 88
integer taborder = 50
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.Retrieve(This.Codigo)
end event

