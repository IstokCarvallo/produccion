$PBExportHeader$w_mant_mues_spro_entidadespacking_multi.srw
forward
global type w_mant_mues_spro_entidadespacking_multi from w_mant_directo
end type
type st_2 from statictext within w_mant_mues_spro_entidadespacking_multi
end type
type dw_2 from datawindow within w_mant_mues_spro_entidadespacking_multi
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_spro_entidadespacking_multi
end type
type st_3 from statictext within w_mant_mues_spro_entidadespacking_multi
end type
type st_4 from statictext within w_mant_mues_spro_entidadespacking_multi
end type
type em_fecha from editmask within w_mant_mues_spro_entidadespacking_multi
end type
type cbx_todos from checkbox within w_mant_mues_spro_entidadespacking_multi
end type
type st_1 from statictext within w_mant_mues_spro_entidadespacking_multi
end type
type em_fechafin from editmask within w_mant_mues_spro_entidadespacking_multi
end type
type st_5 from statictext within w_mant_mues_spro_entidadespacking_multi
end type
type dw_3 from datawindow within w_mant_mues_spro_entidadespacking_multi
end type
type dw_4 from datawindow within w_mant_mues_spro_entidadespacking_multi
end type
type st_6 from statictext within w_mant_mues_spro_entidadespacking_multi
end type
type dw_turnos from datawindow within w_mant_mues_spro_entidadespacking_multi
end type
type dw_5 from datawindow within w_mant_mues_spro_entidadespacking_multi
end type
end forward

global type w_mant_mues_spro_entidadespacking_multi from w_mant_directo
integer width = 4969
integer height = 2068
string title = "MANTENCION DE FAENAS POR PERSONAL/FECHA"
string icon = "AppIcon!"
windowanimationstyle closeanimation = toproll!
st_2 st_2
dw_2 dw_2
uo_selplanta uo_selplanta
st_3 st_3
st_4 st_4
em_fecha em_fecha
cbx_todos cbx_todos
st_1 st_1
em_fechafin em_fechafin
st_5 st_5
dw_3 dw_3
dw_4 dw_4
st_6 st_6
dw_turnos dw_turnos
dw_5 dw_5
end type
global w_mant_mues_spro_entidadespacking_multi w_mant_mues_spro_entidadespacking_multi

type variables
Boolean					ib_limpiarturno, ib_cargafaena
Integer					ii_todasfechas
String					is_error
uo_faenas				iuo_faenas
uo_contratista			iuo_contratista

DatawindowChild		idwc_turnos
end variables

forward prototypes
public function boolean duplicado (string ls_columna, string ls_valor)
protected function boolean wf_actualiza_db ()
public function boolean cambiafaenaactiva ()
public subroutine carganuevafaena ()
end prototypes

public function boolean duplicado (string ls_columna, string ls_valor);DateTime ldt_inicio, ldt_termino
Date		ld_inicio, ld_termino
Time		lt_inicio, lt_termino
String	ls_busqueda
Boolean	lb_respuesta = False
Integer	li_fila


ld_inicio	=	dw_1.Object.enph_fecini[dw_1.GetRow()]
ld_termino	=	dw_1.Object.enph_fecter[dw_1.GetRow()]
lt_inicio	=	dw_1.Object.enph_horini[dw_1.GetRow()]
lt_termino	=	dw_1.Object.enph_horter[dw_1.GetRow()]

SetNull(ldt_inicio)
IF Not IsNull(ld_inicio) AND Not IsNull(lt_inicio) THEN
	ldt_inicio	=	DateTime(ld_inicio, lt_inicio)
	ls_busqueda	=	"DateTime('" + String(ldt_inicio) + "') between dt_fecini and dt_fecter"
	li_fila		=	dw_1.Find(ls_busqueda, 1, dw_1.RowCount())
	
	IF li_fila = dw_1.GetRow() THEN 
		li_fila	=	dw_1.Find(ls_busqueda, li_fila + 1, dw_1.RowCount())
	END IF
									 
	IF li_fila > 0 AND li_fila <> dw_1.GetRow() THEN
		is_error			=	"La Fecha y Hora ("+ String(ldt_inicio) +") de inicio estan dentro de otro rango"
		lb_respuesta	=	True
	END IF
END IF

IF lb_respuesta THEN Return lb_respuesta

SetNull(ldt_termino)
IF Not IsNull(ld_termino) AND Not IsNull(lt_termino) THEN
	ldt_termino	=	DateTime(ld_termino, lt_termino)
	ls_busqueda	=	"DateTime('" + String(ldt_termino) + "') between dt_fecini and dt_fecter"
	li_fila		=	dw_1.Find(ls_busqueda, 1, dw_1.RowCount())
	
	IF li_fila = dw_1.GetRow() THEN 
		li_fila	=	dw_1.Find(ls_busqueda, li_fila + 1, dw_1.RowCount())
		
	END IF
	
	IF li_fila > 0 AND li_fila <> dw_1.GetRow() THEN
		is_error			=	"La Fecha y Hora ("+ String(ldt_termino) +") de término estan dentro de otro rango."
		lb_respuesta	=	True
		
	END IF
END IF

IF lb_respuesta THEN Return lb_respuesta

IF ldt_termino < ldt_inicio THEN
	is_error			=	"La Fecha y Hora de Termino no deben ser inferiores a la fecha y hora de Inicio"
	lb_respuesta  	= 	True
	
END IF

IF lb_respuesta THEN Return lb_respuesta

IF NOT IsNull(ldt_inicio) AND Not IsNull(ldt_termino) THEN
	ls_busqueda	=	"dt_fecini Between DateTime('" + String(ldt_inicio) + "') and DateTime('" + String(ldt_termino) + "') OR " + &
						"dt_fecter Between DateTime('" + String(ldt_inicio) + "') and DateTime('" + String(ldt_termino) + "')"
	li_fila		=	dw_1.Find(ls_busqueda, 1, dw_1.RowCount())
	
	IF li_fila = dw_1.GetRow() THEN 
		li_fila	=	dw_1.Find(ls_busqueda, li_fila + 1, dw_1.RowCount())
		
	END IF
	
	IF li_fila > 0 AND li_fila <> dw_1.GetRow() THEN
		is_error			=	"El rango de esta faena (" + String(ldt_inicio) + " AL " + String(ldt_termino) + ") se topa con otras faenas"
		lb_respuesta	=	True
		
	END IF
	
END IF

Return lb_respuesta
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 THEN
	IF CambiaFaenaActiva() THEN
		 
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
		ELSE
			lb_Retorno	=	True				
			dw_1.ResetUpdate()
		END IF
	ELSE
		RollBack;
		IF sqlca.SQLCode <> 0 THEN 
			F_ErrorBaseDatos(sqlca, This.Title)
		END IF
		lb_Retorno	=	False
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN 
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean cambiafaenaactiva ();Long		ll_plde_codigo, ll_enpa_codper
Integer	li_faen_codigo, li_cont_codigo
String	ls_enpa_rutper, ls_rut
Date 		ld_fecini, ld_fecter

IF dw_1.RowCount() < 1 THEN
	Return False
END IF

ld_fecini			=	Date('01/01/2000')
ld_fecter			=	Date('01/01/2020')
ls_rut				=	dw_1.Object.enpa_rutper[dw_1.RowCount()]
dw_1.DataObject	=	"dw_mues_spro_entidadespacking_histo"
dw_1.SetTransObject(SQLCA)

IF dw_1.Retrieve(uo_selplanta.codigo, ls_rut, ld_fecini, ld_fecter, 1) = -1 THEN
	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					 Information!, RetryCancel!)
	Return False
END IF
			
dw_1.SetSort("enph_fecter asc, enph_horter asc")
dw_1.Sort()

IF dw_1.RowCount() < 1 THEN 
	Return TRUE
	
END IF

ll_plde_codigo	=	dw_1.Object.plde_codigo[dw_1.RowCount()]
ll_enpa_codper	=	dw_1.Object.enpa_codper[dw_1.RowCount()]
li_faen_codigo	=	dw_1.Object.faen_codigo[dw_1.RowCount()]
li_cont_codigo	=	dw_1.Object.cont_codigo[dw_1.RowCount()]
ls_enpa_rutper	=	dw_1.Object.enpa_rutper[dw_1.RowCount()]

DELETE dba.spro_entidadespacking
 WHERE plde_codigo = :ll_plde_codigo
 	AND enpa_rutper = :ls_enpa_rutper;
	 
IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, This.Title)
	RETURN False
	
ELSE
	INSERT INTO dba.spro_entidadespacking
			 (plde_codigo,		 enpa_rutper,	  faen_codigo,		enpa_codper,	 cont_codigo)
	VALUES (:ll_plde_codigo,:ls_enpa_rutper,:li_faen_codigo,:ll_enpa_codper,:li_cont_codigo);
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RETURN False
	ELSE
		RETURN True
	END IF
END IF
end function

public subroutine carganuevafaena ();Date		ld_fecini, ld_fecter
Integer	li_filas, li_recorre, respuesta
String	ls_rut

li_recorre = dw_2.GetSelectedRow(0)

IF li_recorre = 0 THEN
	MessageBox("Error", "Debe seleccionar a lo menos una persona para realizar el cambio de faena", StopSign!)
	Return
END IF

DO WHILE li_recorre > 0
	
	ld_fecini			=	Date('01/01/2000')
	ld_fecter			=	Date('01/01/2020')
	ls_rut				=	dw_2.Object.pers_codigo[li_recorre]
	ib_limpiarturno	=	False
	
	pb_nuevo.TriggerEvent("Clicked")
	
	DO
		IF dw_3.Object.turn_modif[dw_3.GetSelectedRow(0)] <> 1 THEN
			dw_1.DataObject	=	"dw_mues_spro_entidadespacking_histo"
			dw_1.SetTransObject(SQLCA)
			
			IF dw_1.Retrieve(uo_selplanta.codigo, ls_rut, ld_fecini, ld_fecter, 1) = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
								 Information!, RetryCancel!)
			ELSE
				li_filas	=	dw_1.InsertRow(0)
				
				dw_1.SetRow(li_filas)
				dw_1.ScrollToRow(li_filas)
				dw_1.Object.plde_codigo[li_filas]	=	uo_selplanta.codigo
				dw_1.Object.enpa_rutper[li_filas]	=	dw_2.Object.pers_codigo[li_recorre]
				dw_1.Object.faen_codigo[li_filas]	=	dw_3.Object.faen_codigo[dw_3.GetSelectedRow(0)]
				dw_1.Object.enph_fecini[li_filas]	=	dw_3.Object.turn_fecini[dw_3.GetSelectedRow(0)]
				dw_1.Object.enph_fecter[li_filas]	=	dw_3.Object.turn_fecter[dw_3.GetSelectedRow(0)]
				dw_1.Object.enpa_codper[li_filas]	=	dw_2.Object.pers_nrotar[li_recorre]
				dw_1.Object.enph_horini[li_filas]	=	dw_3.Object.turn_horini[dw_3.GetSelectedRow(0)]
				dw_1.Object.enph_horter[li_filas]	=	dw_3.Object.turn_horter[dw_3.GetSelectedRow(0)]
				
				dw_1.AcceptText()
				
				IF Duplicado("", "") THEN
					dw_1.DeleteRow(li_filas)
					dw_2.RowsCopy(li_recorre, li_recorre, Primary!, dw_5, dw_5.RowCount() + 1, Primary!)
					dw_5.Object.enpa_nombre[dw_5.RowCount()]	=	is_error
				END IF
			
				dw_2.SelectRow(li_recorre, False)
				ib_limpiarturno	=	False
				TriggerEvent("ue_guardar")
				dw_1.Reset()
			END IF
		ELSE
			dw_1.DataObject	=	"dw_mues_spro_entidadespacking_histo_especifico"
			dw_1.SetTransObject(SQLCA)
			
			li_filas	=	dw_1.Retrieve(uo_selplanta.codigo, dw_2.Object.pers_codigo[li_recorre], &
											  dw_3.Object.faen_codigo[dw_3.GetSelectedRow(0)], &
											  dw_3.Object.turn_fecini[dw_3.GetSelectedRow(0)], &
											  dw_3.Object.turn_fecter[dw_3.GetSelectedRow(0)], &
											  dw_2.Object.pers_nrotar[li_recorre])
			IF li_filas > 0 THEN
				dw_1.Object.enph_horini[li_filas]	=	dw_3.Object.turn_horini[dw_3.GetSelectedRow(0)]
				dw_1.Object.enph_horter[li_filas]	=	dw_3.Object.turn_horter[dw_3.GetSelectedRow(0)]
			ELSE
				dw_2.RowsCopy(li_recorre, li_recorre, Primary!, dw_5, dw_5.RowCount() + 1, Primary!)
				dw_5.Object.enpa_nombre[dw_5.RowCount()]	=	"Datos para modificar inexistentes"
			END IF
			
			dw_2.SelectRow(li_recorre, False)
			ib_limpiarturno	=	False
			TriggerEvent("ue_guardar")
			dw_1.Reset()
		END IF
	LOOP WHILE respuesta = 1
	
	IF respuesta = 2 THEN 
		Close(This)
	ELSE
		li_recorre = dw_2.GetSelectedRow(li_recorre)
	END IF	
LOOP 

IF dw_5.RowCount() < 1 THEN
	MessageBox("Grabación Exitosa", "Ya no quedan mas registros por corregir", Exclamation!)
	
ELSE
	MessageBox("Problemas en grabación", "Existe personal con problemas de topes horarios, por favor corregir", StopSign!)
END IF

end subroutine

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else						
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	dw_1.SetTransObject(SQLCA)
	dw_2.SetTransObject(SQLCA)
	dw_3.SetTransObject(SQLCA)
	dw_4.SetTransObject(SQLCA)
	
	dw_1.SetRowFocusIndicator(FocusRect!)	
	dw_2.SetRowFocusIndicator(FocusRect!)
	
	iuo_faenas		=	Create uo_faenas
	iuo_contratista	=	Create uo_contratista	
	
	em_fecha.Text								=	String(Today(), 'dd/mm/yyyy')
	
	dw_turnos.GetChild("turn_codigo", idwc_turnos)
	idwc_turnos.SetTransObject(Sqlca)
	idwc_turnos.Retrieve(gstr_paramplanta.codigoplanta)
	dw_turnos.InsertRow(0)
	pb_nuevo.TriggerEvent("Clicked")
END IF
end event

on w_mant_mues_spro_entidadespacking_multi.create
int iCurrent
call super::create
this.st_2=create st_2
this.dw_2=create dw_2
this.uo_selplanta=create uo_selplanta
this.st_3=create st_3
this.st_4=create st_4
this.em_fecha=create em_fecha
this.cbx_todos=create cbx_todos
this.st_1=create st_1
this.em_fechafin=create em_fechafin
this.st_5=create st_5
this.dw_3=create dw_3
this.dw_4=create dw_4
this.st_6=create st_6
this.dw_turnos=create dw_turnos
this.dw_5=create dw_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.dw_2
this.Control[iCurrent+3]=this.uo_selplanta
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.em_fecha
this.Control[iCurrent+7]=this.cbx_todos
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.em_fechafin
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.dw_3
this.Control[iCurrent+12]=this.dw_4
this.Control[iCurrent+13]=this.st_6
this.Control[iCurrent+14]=this.dw_turnos
this.Control[iCurrent+15]=this.dw_5
end on

on w_mant_mues_spro_entidadespacking_multi.destroy
call super::destroy
destroy(this.st_2)
destroy(this.dw_2)
destroy(this.uo_selplanta)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.em_fecha)
destroy(this.cbx_todos)
destroy(this.st_1)
destroy(this.em_fechafin)
destroy(this.st_5)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.st_6)
destroy(this.dw_turnos)
destroy(this.dw_5)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Date		ld_fecini, ld_fecter
Integer	li_filas, respuesta, li_filas2, respuesta2, li_turno

ld_fecini	=	Date(em_fecha.Text)
li_turno		=	dw_turnos.Object.turn_codigo[1]
	
DO
	IF dw_2.Retrieve(uo_selplanta.codigo, ld_fecini, li_turno) = -1 OR &
		dw_3.Retrieve(li_turno, ld_fecini) = -1 OR &
		dw_4.Retrieve(ld_fecini) = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						Information!, RetryCancel!)
	ELSE
		IF dw_2.RowCount() = 0 THEN
			MessageBox("Alerta", "No hay personal asignado al Turno " + String(dw_turnos.Object.turn_codigo[1]) + " en la fecha " + em_fecha.Text)
			Return
	
		ELSE
			li_filas 									=	 dw_4.InsertRow(0)
			dw_4.Object.faen_codigo[li_filas]	=	-1
			dw_4.Object.faen_Nombre[li_filas]	=	'Todas'
			dw_4.SetSort("faen_codigo")
			dw_4.Sort()
			dw_4.SetRow(1)
			dw_4.ScrollToRow(1)
			dw_4.SelectRow(1, True)
			dw_2.SelectRow(0, True)
			
			dw_3.SetRow(1)
			dw_3.ScrollToRow(1)
			dw_3.SelectRow(1, True)
			
			uo_selplanta.Enabled	=	False
			em_fecha.Enabled		=	False
			dw_turnos.Enabled		=	False
			ib_limpiarturno		=	False
			pb_nuevo.TriggerEvent("Clicked")
			
			pb_grabar.Enabled		=	True
			dw_2.Title	=	"Personal Asignado al Turno " + String(dw_turnos.Object.turn_codigo[1]) + " para el día " + em_fecha.Text 
			
		END IF
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(THIS)
end event

event ue_ordenar;String ls_info

str_parms	parm

parm.string_arg[1]	= ordenar
parm.dw_arg				= dw_2

OpenWithParm(w_columna_orden, parm)

ls_info	= Message.StringParm

dw_1.SetRow(1)

RETURN
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_fila

IF ib_cargafaena THEN
	ib_cargafaena	=	False
	CargaNuevaFaena()
	
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

dw_3.Enabled	=	False
dw_2.Enabled	=	False

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event closequery;call super::closequery;IF dw_5.RowCount() > 0 THEN
	IF messagebox("Advertencia", "Si sale de la ventana, quedarán datos con problemas,~r~n¿Desea salir de todas formas?", Question!, YesNo!) = 2 THEN
		Message.ReturnValue	=	1
		RETURN
	END IF
END IF
end event

event resize;st_2.x 		=	st_encabe.x
st_2.y			=	st_encabe.y + st_encabe.Height
st_2.Height 	=	This.WorkSpaceHeight() - st_2.y - 75

st_1.x	=	st_2.x + st_2.Width
st_1.y = st_2.y	
st_1.Height 	=	st_2.Height

dw_2.y		=	st_2.y	+ 20
dw_2.Height =	st_2.Height - 70

dw_3.y		=	dw_2.y

dw_3.Height	=	dw_2.Height / 2

dw_4.y		=	dw_3.y + dw_3.Height 
dw_5.y		=	dw_4.y
dw_4.Height	=	dw_2.Height / 2
dw_5.Height	=	dw_2.Height / 2
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_spro_entidadespacking_multi
integer x = 37
integer y = 36
integer width = 4439
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_spro_entidadespacking_multi
integer x = 4567
integer y = 388
end type

event pb_nuevo::clicked;message.DoubleParm = 0

CHOOSE CASE wf_modifica()
	CASE 0
		Parent.TriggerEvent("ue_guardar")
END CHOOSE

IF message.DoubleParm = -1 THEN RETURN

wf_BloqueaColumnas(False)

dw_1.Reset()

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF ib_limpiarturno THEN 
	IF dw_5.RowCount() > 0 THEN
		IF messagebox("Advertencia", "Si continua con la operación, quedarán datos con problemas,~r~n¿Desea salir de todas formas?", Question!, YesNo!) = 2 THEN
			Message.ReturnValue	=	1
			RETURN
		END IF
	END IF
	uo_SelPlanta.codigo	=	gstr_paramplanta.codigoplanta
	em_fecha.Text			=	String(Today(), 'dd/mm/yyyy')
	
	dw_2.Reset()
	dw_3.Reset()
	dw_4.Reset()
	dw_5.Reset()
	
	uo_SelPlanta.Enabled	=	True
	em_fecha.Enabled		=	True
	dw_turnos.Enabled	=	True
	dw_3.Enabled			=	True
	dw_2.Enabled			=	True
	uo_SelPlanta.TriggerEvent("ue_cambio")
END IF

ib_limpiarturno			=	True
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_spro_entidadespacking_multi
integer x = 4567
integer y = 92
end type

event pb_lectura::clicked;ib_limpiarturno	=	False
Parent.PostEvent("ue_recuperadatos")
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_spro_entidadespacking_multi
boolean visible = false
integer x = 4567
integer y = 972
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_spro_entidadespacking_multi
boolean visible = false
integer x = 4544
integer y = 1212
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_spro_entidadespacking_multi
integer x = 4553
integer y = 1692
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_spro_entidadespacking_multi
integer x = 4567
integer y = 988
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_spro_entidadespacking_multi
boolean visible = false
integer x = 4567
integer y = 688
end type

event pb_grabar::clicked;String ls_mensaje

CHOOSE CASE dw_3.Object.turn_modif[dw_3.GetSelectedRow(0)]
	CASE 1 
		ls_mensaje	=	"¿Esta usted seguro que desea MODIFICAR y no AGREGAR la faena seleccionada?"
		
	CASE 0
		ls_mensaje	=	"¿Esta usted seguro que desea AGREGAR y no MODIFICAR la faena seleccionada?"
		
END CHOOSE

IF MessageBox("Confirmación", ls_mensaje, Question!, YesNo!, 2) = 2 THEN Return

ib_cargafaena	=	True
Parent.TriggerEvent("ue_guardar")
end event

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_spro_entidadespacking_multi
boolean visible = false
integer x = 0
integer y = 0
integer width = 4480
integer height = 788
string title = "Maestro Historico de Personal"
string dataobject = "dw_mues_spro_entidadespacking_histo_especifico"
boolean border = true
boolean livescroll = false
borderstyle borderstyle = StyleBox!
end type

event dw_1::itemchanged;call super::itemchanged;Integer 				li_null
String				ls_columna

SetNull(li_null)
ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "faen_codigo"
		IF NOT iuo_faenas.existe(Integer(data), True, SQLCA) THEN
			dw_1.object.faen_codigo[il_fila]	=	li_null
			Return 1
		END IF
		
	CASE "cont_codigo"
		IF NOT iuo_contratista.existe(Integer(data), True, SQLCA) THEN
			dw_1.object.cont_codigo[il_fila]	=	li_null
			Return 1
		END IF
		
	CASE "enph_fecini"
		IF Duplicado(ls_columna, data) THEN
			dw_1.Object.enph_fecini[Row]	=	Date(li_null)
			Return 1
		END IF
		
	CASE "enph_fecter"
		IF Duplicado(ls_columna, data) THEN
			dw_1.Object.enph_fecter[Row]	=	Date(li_null)
			Return 1
		END IF
		
	CASE "enph_horini"
		IF Duplicado(ls_columna, data) THEN
			dw_1.Object.enph_horini[Row]	=	Time(li_null)
			Return 1
		END IF
		
	CASE "enph_horter"
		IF Duplicado(ls_columna, data) THEN
			dw_1.Object.enph_horter[Row]	=	Time(li_null)
			Return 1
		END IF
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

type st_2 from statictext within w_mant_mues_spro_entidadespacking_multi
integer x = 37
integer y = 212
integer width = 1701
integer height = 1728
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

type dw_2 from datawindow within w_mant_mues_spro_entidadespacking_multi
integer x = 73
integer y = 232
integer width = 1646
integer height = 1684
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "Personal Asignado a Turnos"
string dataobject = "dw_mant_turnos_personal_dia_turno"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;Date		ld_fecini, ld_fecter
Integer	li_filas, respuesta
String		ls_rut

IF Row = 0 THEN 
	This.SelectRow(0, False)
	RETURN
END IF

This.SelectRow(Row, NOT This.IsSelected(Row))
end event

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_spro_entidadespacking_multi
event destroy ( )
integer x = 910
integer y = 88
integer height = 80
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		idwc_turnos.Retrieve(-1)
	Case Else
		idwc_turnos.Retrieve(This.Codigo)
		
End Choose
end event

type st_3 from statictext within w_mant_mues_spro_entidadespacking_multi
integer x = 704
integer y = 92
integer width = 192
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_spro_entidadespacking_multi
integer x = 2126
integer y = 92
integer width = 197
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
string text = "Fecha"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_spro_entidadespacking_multi
integer x = 2327
integer y = 80
integer width = 503
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
textcase textcase = lower!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type cbx_todos from checkbox within w_mant_mues_spro_entidadespacking_multi
boolean visible = false
integer x = 5490
integer y = 1484
integer width = 416
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;em_fecha.Enabled		=	Not This.Checked
em_fechafin.Enabled	=	Not This.Checked

IF This.Checked THEN
	em_fecha.Text		=	'01.01.1900'
	em_fechafin.Text	=	String(Today(), 'dd/mm/yyyy')
	ii_todasfechas		=	1
ELSE
	ii_todasfechas		=	0
END IF
end event

type st_1 from statictext within w_mant_mues_spro_entidadespacking_multi
integer x = 1737
integer y = 212
integer width = 2738
integer height = 1728
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

type em_fechafin from editmask within w_mant_mues_spro_entidadespacking_multi
boolean visible = false
integer x = 5490
integer y = 1484
integer width = 416
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
textcase textcase = lower!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_5 from statictext within w_mant_mues_spro_entidadespacking_multi
boolean visible = false
integer x = 5490
integer y = 1484
integer width = 416
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Al"
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_mant_mues_spro_entidadespacking_multi
integer x = 1760
integer y = 232
integer width = 2683
integer height = 888
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "Faenas Disponibles a Asignación"
string dataobject = "dw_mues_faenasturno_fecha"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;This.SetRow(Row)
This.ScrollToRow(Row)
This.SelectRow(0, False)
This.SelectRow(Row, True)
end event

type dw_4 from datawindow within w_mant_mues_spro_entidadespacking_multi
integer x = 1760
integer y = 1128
integer width = 905
integer height = 788
integer taborder = 40
boolean bringtotop = true
boolean titlebar = true
string title = "Filtro de Faenas"
string dataobject = "dw_mues_faenasturno_fecha_solofaena"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event doubleclicked;Integer	li_fila

This.SelectRow(0, False)
This.SelectRow(Row, True)

dw_2.SetFilter(String(This.Object.faen_codigo[row]) + " in (Faen_codigo, -1)")
dw_2.Filter()

IF dw_2.RowCount() < 1 THEN
	MessageBox("Filtro", "No se encuentra Personal con la faena " + String(This.Object.faen_nombre[row]) + " asignada.~r~n" + &
						      "Se mostrará todo el Personal", Information!)
	
	li_fila	=	This.Find("faen_codigo = -1", 1, This.RowCount() + 1)
	
	This.SelectRow(0, False)
	This.SelectRow(li_fila, True)
	This.SetRow(li_fila)
	This.ScrollToRow(li_fila)
	
	dw_2.SetFilter("-1 in (Faen_codigo, -1)")
	dw_2.Filter()
END IF

dw_2.SelectRow(0, True)
end event

type st_6 from statictext within w_mant_mues_spro_entidadespacking_multi
integer x = 2939
integer y = 92
integer width = 197
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
string text = "Turno"
boolean focusrectangle = false
end type

type dw_turnos from datawindow within w_mant_mues_spro_entidadespacking_multi
integer x = 3163
integer y = 84
integer width = 946
integer height = 92
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_turnos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_mant_mues_spro_entidadespacking_multi
integer x = 2665
integer y = 1128
integer width = 1778
integer height = 788
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "Personal Con Problemas al asignar"
string dataobject = "dw_mant_turnos_personal_dia_turno"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;
IF row < 1 THEN Return
This.SelectRow(0, False)
This.SelectRow(Row, Not This.IsSelected(Row))

IF This.IsSelected(Row) THEN
	w_main.SetMicroHelp(This.Object.enpa_nombre[Row])
	
ELSE
	w_main.SetMicroHelp("Listo...")
	
END IF
end event

event doubleclicked;Date		ld_fecini, ld_fecter
Integer	li_filas, li_recorre, respuesta
String	ls_rut
str_mant	lstr_mant


IF Row < 1 THEN RETURN

li_recorre = Row

ld_fecini			=	Date('01/01/2000')
ld_fecter			=	Date('01/01/2020')
ls_rut				=	This.Object.pers_codigo[li_recorre]
dw_1.Reset()
lstr_mant.dw		=	dw_1

DO
	IF dw_1.Retrieve(uo_selplanta.codigo, ls_rut, ld_fecini, ld_fecter, 1) = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						 Information!, RetryCancel!)
	ELSE
		li_filas										=	dw_1.InsertRow(0)
		dw_1.SetRow(li_filas)
		dw_1.ScrollToRow(li_filas)
		dw_1.Object.plde_codigo[li_filas]	=	uo_selplanta.codigo
		dw_1.Object.enpa_rutper[li_filas]	=	This.Object.pers_codigo[li_recorre]
		dw_1.Object.enpa_codper[li_filas]	=	This.Object.pers_nrotar[li_recorre]
		dw_1.Object.faen_codigo[li_filas]	=	dw_3.Object.faen_codigo[dw_3.GetSelectedRow(0)]
		dw_1.Object.enph_fecini[li_filas]	=	dw_3.Object.turn_fecini[dw_3.GetSelectedRow(0)]
		dw_1.Object.enph_fecter[li_filas]	=	dw_3.Object.turn_fecter[dw_3.GetSelectedRow(0)]
		dw_1.Object.enph_horini[li_filas]	=	dw_3.Object.turn_horini[dw_3.GetSelectedRow(0)]
		dw_1.Object.enph_horter[li_filas]	=	dw_3.Object.turn_horter[dw_3.GetSelectedRow(0)]
		
		dw_1.AcceptText()
		
		OpenWithParm(w_mant_deta_faenashistoricas, lstr_mant)
		
	END IF
LOOP WHILE respuesta = 1

dw_1.Reset()
This.DeleteRow(Row)

IF This.RowCount() < 1 THEN
	MessageBox("Grabación Exitosa", "Ya no quedan mas registros por corregir", Exclamation!)
	pb_nuevo.TriggerEvent(Clicked!)
	pb_nuevo.TriggerEvent(Clicked!)
	
END IF
end event

