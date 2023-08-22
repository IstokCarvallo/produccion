$PBExportHeader$w_mant_deta_faenashistoricas.srw
forward
global type w_mant_deta_faenashistoricas from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_faenashistoricas from w_mant_detalle_csd
integer width = 3182
integer height = 1248
end type
global w_mant_deta_faenashistoricas w_mant_deta_faenashistoricas

forward prototypes
public function boolean cambiafaenaactiva ()
public function boolean duplicado (string ls_columna, string ls_valor)
public function boolean wf_actualizadb ()
end prototypes

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

public function boolean wf_actualizadb ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then
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

		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)

		lb_Retorno	=	False
	END IF
ELSE
	RollBack;

	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)

	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_mant_deta_faenashistoricas.create
call super::create
end on

on w_mant_deta_faenashistoricas.destroy
call super::destroy
end on

event open;call super::open;dw_1.Object.faen_codigo.Protect	=	1
dw_1.Object.cont_codigo.Protect	=	1
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

SetPointer(HourGlass!)

Message.DoubleParm = 0

w_main.SetMicroHelp("Grabando información...")
TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN 
	RETURN
ELSE
	IF wf_actualizadb() THEN
		Message.DoubleParm = 0
	ELSE
		Message.DoubleParm = -1
	END IF
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;IF Duplicado("", "") THEN
	Message.DoubleParm = -1
ELSE
	Message.DoubleParm = 1
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_faenashistoricas
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_faenashistoricas
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_faenashistoricas
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_faenashistoricas
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_faenashistoricas
integer x = 2889
end type

event pb_cancela::clicked;
IF MessageBox("Confirmación", "¿Esta seguro de anular la solicitud de cambio de faena para esta persona?", Question!, YesNo!, 2) = 1 THEN
	istr_mant.respuesta = 2
	
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_faenashistoricas
integer x = 2889
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_faenashistoricas
integer x = 2889
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_faenashistoricas
integer width = 2702
integer height = 1044
string dataobject = "dw_mues_spro_entidadespacking_histo"
end type

event dw_1::itemchanged;call super::itemchanged;Integer 				li_null
String				ls_columna

SetNull(li_null)
ls_columna = dwo.name

CHOOSE CASE ls_columna		
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

