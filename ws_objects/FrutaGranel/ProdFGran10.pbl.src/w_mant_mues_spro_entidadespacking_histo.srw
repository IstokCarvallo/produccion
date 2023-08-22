$PBExportHeader$w_mant_mues_spro_entidadespacking_histo.srw
forward
global type w_mant_mues_spro_entidadespacking_histo from w_mant_directo
end type
type st_2 from statictext within w_mant_mues_spro_entidadespacking_histo
end type
type dw_2 from datawindow within w_mant_mues_spro_entidadespacking_histo
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_spro_entidadespacking_histo
end type
type st_3 from statictext within w_mant_mues_spro_entidadespacking_histo
end type
type st_4 from statictext within w_mant_mues_spro_entidadespacking_histo
end type
type em_fecha from editmask within w_mant_mues_spro_entidadespacking_histo
end type
type cbx_todos from checkbox within w_mant_mues_spro_entidadespacking_histo
end type
type st_1 from statictext within w_mant_mues_spro_entidadespacking_histo
end type
type em_fechafin from editmask within w_mant_mues_spro_entidadespacking_histo
end type
type st_5 from statictext within w_mant_mues_spro_entidadespacking_histo
end type
end forward

global type w_mant_mues_spro_entidadespacking_histo from w_mant_directo
integer width = 4914
integer height = 2272
string title = "MANTENCION DE FAENAS POR PERSONAL/FECHA"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
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
end type
global w_mant_mues_spro_entidadespacking_histo w_mant_mues_spro_entidadespacking_histo

type variables
Boolean				ib_limpiarturno
Integer				ii_todasfechas
uo_faenas			iuo_faenas
uo_contratista		iuo_contratista
end variables

forward prototypes
public function boolean duplicado (string ls_columna, string ls_valor)
protected function boolean wf_actualiza_db ()
public function boolean cambiafaenaactiva ()
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

CHOOSE CASE ls_columna
	CASE "enph_fecini"
		ld_inicio	=	Date(ls_valor)
		
	CASE "enph_fecter"
		ld_termino	=	Date(ls_valor)
		
	CASE "enph_horini"
		lt_inicio	=	Time(ls_valor)
		
	CASE "enph_horter"
		lt_termino	=	Time(ls_valor)

END CHOOSE

SetNull(ldt_inicio)
IF Not IsNull(ld_inicio) AND Not IsNull(lt_inicio) THEN
	ldt_inicio	=	DateTime(ld_inicio, lt_inicio)
	IF ld_inicio < Date(em_fecha.Text) THEN
		MessageBox("Integridad de Datos", "El inicio de la faena no puede ser inferior a la fecha de inicio del filtro", StopSign!)
		
		lb_respuesta	=	True
	ELSE
		ls_busqueda	=	"DateTime('" + String(ldt_inicio) + "') between dt_fecini and dt_fecter"
		li_fila		=	dw_1.Find(ls_busqueda, 1, dw_1.RowCount())
		
		IF li_fila = dw_1.GetRow() THEN 
			li_fila	=	dw_1.Find(ls_busqueda, li_fila + 1, dw_1.RowCount())
		END IF
										 
		IF li_fila > 0 AND li_fila <> dw_1.GetRow() THEN
			MessageBox("Integridad de Datos", "La Fecha y Hora ("+ String(ldt_inicio) +") de inicio estan dentro de otro rango" + &
						  "~r~nPor favor revisar", StopSign!)
			lb_respuesta	=	True
		END IF
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
		MessageBox("Integridad de Datos", "La Fecha y Hora ("+ String(ldt_termino) +") de término estan dentro de otro rango." + &
					  "~r~nPor favor revisar", StopSign!)
		lb_respuesta	=	True
		
	END IF
END IF

IF lb_respuesta THEN Return lb_respuesta

IF ldt_termino < ldt_inicio THEN
	MessageBox("Integridad de Datos", "La Fecha y Hora de Termino no deben ser inferiores a la fecha y hora de Inicio", StopSign!)
	lb_respuesta  = True
	
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
		MessageBox("Integridad de Datos", "El rango de esta faena (" + String(ldt_inicio) + " AL " + String(ldt_termino) + &
					  ") se topa con otras faenas.~r~nPor favor revisar", StopSign!)
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
String	ls_enpa_rutper

dw_1.SetSort("enph_fecter asc, enph_horter asc")
dw_1.Sort()

ll_plde_codigo	=	dw_1.Object.plde_codigo[dw_1.RowCount()]
ll_enpa_codper	=	dw_1.Object.enpa_codper[dw_1.RowCount()]
li_faen_codigo	=	dw_1.Object.faen_codigo[dw_1.RowCount()]
li_cont_codigo	=	dw_1.Object.cont_codigo[dw_1.RowCount()]
ls_enpa_rutper	=	dw_1.Object.enpa_rutper[dw_1.RowCount()]


DELETE dbo.spro_entidadespacking
 WHERE plde_codigo = :ll_plde_codigo
 	AND enpa_rutper = :ls_enpa_rutper;
	 
IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, This.Title)
	RETURN False
	
ELSE
	INSERT INTO dbo.spro_entidadespacking
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

event open;Boolean	lb_Cerrar

IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(THIS)
	
ELSE
	x				= 	0
	y				= 	0
	im_menu		=	m_principal
	
	This.ParentWindow().ToolBarVisible	=	True
	im_menu.Item[1].Item[6].Enabled		=	True
	im_menu.Item[7].Visible					=	False
	This.Icon									=	Gstr_apl.Icono
	
	dw_1.SetTransObject(sqlca)
	dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
	
	istr_mant.UsuarioSoloConsulta			=	OpcionSoloConsulta()
	istr_mant.Solo_Consulta					=	istr_mant.UsuarioSoloConsulta
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
									
	buscar			= "Rut:Spers_codigo,Nombre:Senpa_nombre"
	ordenar			= "Rut:pers_codigo,Nombre:enpa_nombre"
	is_ultimacol	= "enpa_nombre"

	uo_SelPlanta.Seleccion(False, False)
	dw_2.SetTransObject(SQLCA)
	
	dw_1.SetRowFocusIndicator(FocusRect!)	
	dw_2.SetRowFocusIndicator(FocusRect!)
	
	iuo_faenas				=	Create uo_faenas
	iuo_contratista		=	Create uo_contratista	
	
	uo_SelPlanta.codigo	=	gstr_paramplanta.codigoplanta
	em_fecha.Text			=	String(Today(), 'dd/mm/yyyy')

	uo_SelPlanta.TriggerEvent("ue_cambio")
	cbx_todos.TriggerEvent("Clicked")
	pb_nuevo.TriggerEvent("Clicked")
END IF
end event

on w_mant_mues_spro_entidadespacking_histo.create
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
end on

on w_mant_mues_spro_entidadespacking_histo.destroy
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
end on

event resize;//
end event

event ue_recuperadatos;call super::ue_recuperadatos;Date		ld_fecini, ld_fecter
Integer	li_filas, respuesta, li_filas2, respuesta2 

ld_fecini	=	Date(em_fecha.Text)
ld_fecter	=	Date(em_fechafin.Text)
	
DO
	IF dw_2.Retrieve(uo_selplanta.codigo, ld_fecini, ld_fecter) = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						Information!, RetryCancel!)
	ELSE
		IF dw_2.RowCount() = 0 THEN
			MessageBox("Alerta", "No hay personal asignado a un turno para el rango " + em_fecha.Text + " al " + em_fechaFin.Text)
			Return
	
		ELSE
			uo_selplanta.Enabled	=	False
			em_fecha.Enabled		=	False
			em_fechaFin.Enabled	=	False
			cbx_todos.Enabled		=	False
			ib_limpiarturno		=	False
			pb_nuevo.TriggerEvent("Clicked")
			
			dw_2.Title	=	"Personal Asignado a Turnos Desde " + em_fecha.Text + " Al " + em_fechaFin.Text
			
			DO
				IF dw_1.Retrieve(uo_selplanta.codigo, dw_2.Object.pers_codigo[1], ld_fecini, ld_fecter, ii_todasfechas) = -1 THEN
					respuesta2 = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
									 Information!, RetryCancel!)
				ELSE
					li_filas					=	dw_1.RowCount()
					pb_imprimir.Enabled	=	li_filas > 0
					pb_grabar.Enabled		=	li_filas > 0
					pb_eliminar.Enabled	=	li_filas > 0
					pb_insertar.Enabled	=	True
					
					dw_1.Title				=	"Detalle Faenas Para Personal " + dw_2.Object.compute_1[1]
					
					dw_1.SetRow(1)
					dw_2.SetRow(1)
					dw_2.SelectRow(0, False)
					dw_2.SelectRow(1, True)
				END IF
			LOOP WHILE respuesta2 = 1
			
			IF respuesta2 = 2 THEN Close(THIS)
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

FOR li_fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(li_fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.plde_codigo[li_fila]	=	uo_selplanta.codigo
		dw_1.Object.enpa_rutper[li_fila]	=	dw_2.Object.pers_codigo[dw_2.GetRow()]
		dw_1.Object.enpa_codper[li_fila]	=	dw_2.Object.pers_nrotar[dw_2.GetRow()]
		
	END IF
NEXT
end event

event ue_imprimir;SetPointer(HourGlass!)
Date		ld_fecini, ld_fecter
Long		fila
String	ls_rut

ld_fecini	=	Date(em_fecha.Text)
ld_fecter	=	Date(em_fechafin.Text)

IF MessageBox("Confirmación de impresión", "[Si] Para el personal seleccionado.~r~n[No] Para todo el personal.~r~n¿Que desea imprimir'", Question!, YesNo!) = 1 THEN
	ls_rut		=	dw_2.Object.pers_codigo[dw_2.GetRow()]
ELSE
	ls_rut 		= 	"-1"
END IF

str_info	lstr_info

lstr_info.titulo	= "MAESTRO HISTORICO DE FAENAS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_spro_entidadespacking_histo"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_selplanta.codigo, ls_rut, ld_fecini, ld_fecter, ii_todasfechas)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)	
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF

END IF

SetPointer(Arrow!)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_spro_entidadespacking_histo
integer x = 37
integer y = 36
integer width = 4439
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_spro_entidadespacking_histo
integer x = 4558
integer y = 424
end type

event pb_nuevo::clicked;call super::clicked;IF ib_limpiarturno THEN 
	uo_SelPlanta.codigo	=	gstr_paramplanta.codigoplanta
	em_fecha.Text			=	String(Today(), 'dd/mm/yyyy')
	
	dw_2.Reset()
	uo_SelPlanta.Enabled	=	True
	em_fecha.Enabled		=	True
	em_fechaFin.Enabled	=	True
	cbx_todos.Enabled		=	True
	
	uo_SelPlanta.TriggerEvent("ue_cambio")
	cbx_todos.TriggerEvent("Clicked")
END IF

ib_limpiarturno			=	True
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_spro_entidadespacking_histo
integer x = 4558
integer y = 180
end type

event pb_lectura::clicked;ib_limpiarturno	=	False
Parent.PostEvent("ue_recuperadatos")
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_spro_entidadespacking_histo
integer x = 4558
integer y = 896
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_spro_entidadespacking_histo
integer x = 4558
integer y = 660
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_spro_entidadespacking_histo
integer x = 4558
integer y = 1872
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_spro_entidadespacking_histo
integer x = 4558
integer y = 1388
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_spro_entidadespacking_histo
integer x = 4558
integer y = 1144
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_spro_entidadespacking_histo
integer x = 1733
integer y = 232
integer width = 2720
integer height = 1900
boolean titlebar = true
string title = "Detalle Faenas"
string dataobject = "dw_mues_spro_entidadespacking_histo"
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

type st_2 from statictext within w_mant_mues_spro_entidadespacking_histo
integer x = 37
integer y = 212
integer width = 1682
integer height = 1944
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_mant_mues_spro_entidadespacking_histo
integer x = 73
integer y = 232
integer width = 1623
integer height = 1900
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "Personal Asignado a Turnos"
string dataobject = "dw_mant_turnos_personal_dia"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;Date		ld_fecini, ld_fecter
Integer	li_filas, respuesta
String	ls_rut

IF Row < 1 THEN RETURN

ld_fecini			=	Date(em_fecha.Text)
ld_fecter			=	Date(em_fechafin.Text)
ib_limpiarturno	=	False
ls_rut				=	dw_2.Object.pers_codigo[Row]

Parent.pb_nuevo.TriggerEvent("Clicked")

DO
	IF dw_1.Retrieve(uo_selplanta.codigo, ls_rut, ld_fecini, ld_fecter, ii_todasfechas) = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						 Information!, RetryCancel!)
	ELSE
		li_filas					=	dw_1.RowCount()
		pb_imprimir.Enabled	=	li_filas > 0
		pb_grabar.Enabled		=	li_filas > 0
		pb_eliminar.Enabled	=	li_filas > 0
		pb_insertar.Enabled	=	True
		
		dw_1.Title				=	"Detalle Faenas Para Personal " + dw_2.Object.compute_1[Row]
		
		dw_1.SetRow(1)
		dw_2.SetRow(Row)
		dw_2.SelectRow(0, False)
		dw_2.SelectRow(Row, True)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(Parent)
end event

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_spro_entidadespacking_histo
event destroy ( )
integer x = 1234
integer y = 88
integer height = 80
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_3 from statictext within w_mant_mues_spro_entidadespacking_histo
integer x = 1029
integer y = 92
integer width = 192
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
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_spro_entidadespacking_histo
integer x = 2450
integer y = 92
integer width = 197
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
string text = "Fecha"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_spro_entidadespacking_histo
integer x = 2651
integer y = 80
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

type cbx_todos from checkbox within w_mant_mues_spro_entidadespacking_histo
integer x = 3630
integer y = 96
integer width = 265
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
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

type st_1 from statictext within w_mant_mues_spro_entidadespacking_histo
integer x = 1719
integer y = 212
integer width = 2757
integer height = 1944
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fechafin from editmask within w_mant_mues_spro_entidadespacking_histo
integer x = 3186
integer y = 80
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

type st_5 from statictext within w_mant_mues_spro_entidadespacking_histo
integer x = 3099
integer y = 92
integer width = 87
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
string text = "Al"
boolean focusrectangle = false
end type

