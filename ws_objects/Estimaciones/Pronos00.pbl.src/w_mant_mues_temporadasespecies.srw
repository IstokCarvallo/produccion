$PBExportHeader$w_mant_mues_temporadasespecies.srw
forward
global type w_mant_mues_temporadasespecies from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_temporadasespecies
end type
type uo_seltemporada from uo_seleccion_paramtemporada within w_mant_mues_temporadasespecies
end type
end forward

global type w_mant_mues_temporadasespecies from w_mant_tabla
integer width = 2510
string title = "Temporadas por Especie"
string icon = "AppIcon!"
st_1 st_1
uo_seltemporada uo_seltemporada
end type
global w_mant_mues_temporadasespecies w_mant_mues_temporadasespecies

type variables
w_mant_deta_temporadaespecie iw_mantencion

uo_Temporada	iuo_Temporada
end variables

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True
istr_Mant.Argumento[1]	=	String(uo_SelTemporada.Codigo)

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

lstr_info.titulo	= "MAESTRO DE TEMPORADAS ESPECIES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_temporadaespecie"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelTemporada.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve(uo_SelTemporada.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
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

on w_mant_mues_temporadasespecies.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_seltemporada=create uo_seltemporada
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_seltemporada
end on

on w_mant_mues_temporadasespecies.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_seltemporada)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra		= True
istr_mant.agrega	= False
istr_Mant.Argumento[1]	=	String(uo_SelTemporada.Codigo)

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

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelTemporada.Codigo) Then lb_Cerrar = True

IF lb_Cerrar Then
	Close(This)
Else
	uo_SelTemporada.Seleccion(False, False)
	iuo_Temporada	=	Create uo_Temporada
	
	If iuo_Temporada.Vigente(False, Sqlca) Then
		uo_SelTemporada.dw_Seleccion.Object.Codigo[1]	= iuo_Temporada.Temporada
		uo_SelTemporada.Codigo									= iuo_Temporada.Temporada
	End If

	buscar	= "Código:Nagro_codigo,Descripción:Sagro_nombre"
	ordenar	= "Código:agro_codigo,Descripción:agro_nombre"
End If
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	istr_Mant.Argumento[1]	=	String(uo_SelTemporada.Codigo)

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	BuscaAgronomo()
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_temporadasespecies
integer y = 356
integer width = 1815
integer height = 1456
string dataobject = "dw_mues_temporadaespecie"
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_temporadasespecies
integer x = 82
integer width = 1815
integer height = 272
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_temporadasespecies
integer x = 2071
integer y = 140
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_temporadasespecies
boolean visible = false
integer x = 2043
integer y = 428
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_temporadasespecies
integer x = 2071
integer y = 608
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_temporadasespecies
integer x = 2071
integer y = 784
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_temporadasespecies
integer x = 2071
integer y = 988
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_temporadasespecies
integer x = 2071
integer y = 1152
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_temporadasespecies
integer x = 2071
integer y = 1540
end type

type st_1 from statictext within w_mant_mues_temporadasespecies
integer x = 320
integer y = 176
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Temporada"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_seltemporada from uo_seleccion_paramtemporada within w_mant_mues_temporadasespecies
integer x = 709
integer y = 168
integer height = 80
integer taborder = 20
boolean bringtotop = true
end type

on uo_seltemporada.destroy
call uo_seleccion_paramtemporada::destroy
end on

