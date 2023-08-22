$PBExportHeader$w_mant_spro_gruasporturno.srw
$PBExportComments$Ventana Mantenedor de Peso Estandar por Especie.
forward
global type w_mant_spro_gruasporturno from w_mant_directo
end type
type st_2 from statictext within w_mant_spro_gruasporturno
end type
type st_1 from statictext within w_mant_spro_gruasporturno
end type
type em_fecha from editmask within w_mant_spro_gruasporturno
end type
type uo_selplantas from uo_seleccion_plantas within w_mant_spro_gruasporturno
end type
end forward

global type w_mant_spro_gruasporturno from w_mant_directo
integer width = 3063
integer height = 1816
string title = "MAESTRO DE TARA DE GRUAS"
st_2 st_2
st_1 st_1
em_fecha em_fecha
uo_selplantas uo_selplantas
end type
global w_mant_spro_gruasporturno w_mant_spro_gruasporturno

type variables
uo_temporada		iuo_temporada
uo_turno				iuo_turno
uo_grua				iuo_grua


Integer    			ii_tipo
Date					id_FechaInicio
Time					id_horaactual
end variables

forward prototypes
public function boolean existefecha (string as_fecha)
public function boolean duplicado (string as_columna, string as_valor)
end prototypes

public function boolean existefecha (string as_fecha);Integer	li_Existe, li_planta
DateTime	ldt_Fecha
Boolean	lb_Retorno = True

RETURN lb_Retorno
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
Integer	li_grua, li_turno
Date		ld_fecha
Time		lt_hora


li_grua				=	dw_1.Object.grua_codigo[il_Fila]
ld_fecha				=	id_FechaInicio
lt_hora				=	dw_1.Object.tagr_hordes[il_fila]

CHOOSE CASE as_Columna
	CASE "grua_codigo"
		li_grua	=	Integer(as_valor)
	
	CASE "tagr_hordes"
		lt_hora	=	Time(as_valor)
		
END CHOOSE

ll_Fila	=	dw_1.Find("grua_codigo = " + String(li_grua)	+ " AND " + &
							 "tagr_hordes = Time('" + String(lt_hora)		+ "') AND " + &
							 "date(tagr_fecdes) = date('" + String(ld_fecha) 	+ "')", 1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresado anteriormente", Information!, Ok!)
	RETURN True
	
ELSE
	RETURN False
	
END IF
end function

on w_mant_spro_gruasporturno.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.em_fecha=create em_fecha
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_fecha
this.Control[iCurrent+4]=this.uo_selplantas
end on

on w_mant_spro_gruasporturno.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.em_fecha)
destroy(this.uo_selplantas)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	iuo_temporada		=	Create uo_temporada
	iuo_turno			=	Create uo_turno
	iuo_grua				=	Create uo_grua
	
	uo_SelPlantas.Seleccion(False, False)
	uo_SelPlantas.Inicia(gstr_paramplanta.CodigoPlanta)
	
	id_FechaInicio				=	Date(f_fechaHora())
	id_horaactual				=	Time(f_fechaHora())
	em_fecha.Text				=	String(id_fechainicio)
	ias_campo[1]				= 	""
	ias_campo[2]				= 	""
	
	buscar						= 	"Grua:Ngrua_codigo"
	ordenar						= 	"Grua:grua_codigo"
End If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila, respuesta
Integer	li_Especie
DateTime	ldt_FechaInicio

dw_1.Setredraw(FALSE)

DO
	ll_fila	= dw_1.Retrieve(uo_SelPlantas.Codigo, id_FechaInicio)

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.setsort("enva_tipoen A, enva_codigo A, pees_fecini D")
		dw_1.Sort()		
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		pb_imprimir.Enabled	= True
		il_fila					= 1

	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

dw_1.SetRedraw(TRUE)

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetColumn("grua_codigo")

id_FechaInicio				=	Date(f_fechaHora())
id_horaactual				=	Time(f_fechaHora())

dw_1.Object.tagr_fecdes[il_fila]	=	id_fechainicio
dw_1.Object.tagr_hordes[il_fila]	=	id_horaactual

dw_1.SetItemStatus(il_fila, 0, Primary!, New!)
end event

event ue_antesguardar;call super::ue_antesguardar;Long  ll_fila
Integer li_tipenv1, li_envase1, li_tipenv2, li_envase2
Datetime ld_FechaTerminoSgte
Boolean	lb_respuesta

FOR il_fila = 1 TO dw_1.RowCount()
	Message.DoubleParm = 1
	TriggerEvent("ue_validaregistro")
	IF Message.DoubleParm = -1 THEN 
		dw_1.setRow(il_fila)
		RETURN
	END IF	
NEXT

FOR ll_fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.plde_codigo[ll_fila]	=	Integer(istr_mant.argumento[1])
		
	END IF
NEXT

dw_1.SetRedraw(TRUE)
end event

event ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]
Date		ld_fecha
Time		lt_hora

IF Isnull(dw_1.Object.grua_codigo[il_fila]) OR dw_1.Object.grua_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Grua."
	ls_colu[li_cont]	= "grua_codigo"
END IF

IF Isnull(dw_1.Object.tagr_fecdes[il_fila]) OR dw_1.Object.tagr_fecdes[il_fila] = ld_fecha THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha de Tara."
	ls_colu[li_cont]	= "tagr_fecha"
	
ELSEIF dw_1.Object.tagr_fecdes[il_fila] <> id_FechaInicio THEN
	dw_1.Object.tagr_fecdes[il_fila]	=	id_FechaInicio
	
END IF

IF Isnull(dw_1.Object.tagr_hordes[il_fila]) OR dw_1.Object.tagr_hordes[il_fila] = lt_hora THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nHora Asignado."
	ls_colu[li_cont]	= "tagr_hordes"
END IF

IF Isnull(dw_1.Object.tagr_kiltar[il_fila]) OR dw_1.Object.tagr_kiltar[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTara Grua."
	ls_colu[li_cont]	= "tagr_kiltar"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de:" + ls_mensaje, StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "DATOS TARA DE GRUAS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_spro_gruasporturno"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelPlantas.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.",  StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_spro_gruasporturno
integer width = 2373
integer height = 220
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_spro_gruasporturno
integer x = 2697
integer y = 368
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;uo_SelPlantas.Bloquear(True)



end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_spro_gruasporturno
integer x = 2697
integer taborder = 40
end type

event pb_lectura::clicked;call super::clicked;uo_SelPlantas.Bloquear(True)


end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_spro_gruasporturno
integer x = 2697
integer y = 724
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_spro_gruasporturno
integer x = 2697
integer y = 544
integer taborder = 70
end type

event pb_insertar::clicked;call super::clicked;Pb_grabar.Enabled	=	True
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_spro_gruasporturno
integer x = 2697
integer y = 1400
integer taborder = 110
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_spro_gruasporturno
integer x = 2697
integer y = 1084
integer taborder = 100
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_spro_gruasporturno
integer x = 2697
integer y = 904
integer taborder = 90
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_spro_gruasporturno
integer y = 296
integer width = 2373
integer height = 1360
integer taborder = 50
string dataobject = "dw_mues_spro_gruasporturno"
end type

event dw_1::itemchanged;String	ls_Columna, ls_Nula
Date	 	ld_fecha

SetNull(ls_Nula)
SetNull(ld_fecha)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "grua_codigo"
		IF Duplicado(ls_Columna, Data) THEN
			This.Object.grua_codigo[il_Fila]	=	Integer(ls_Nula)
			RETURN 1
		ELSE
			IF NOT iuo_grua.Existe(Integer(istr_Mant.Argumento[1]), integer(data), True, Sqlca) THEN
				This.Object.grua_codigo[il_Fila]	=	Integer(ls_Nula)
				RETURN 1
			ELSE
				This.Object.grua_descri[il_Fila]	=	iuo_grua.grua_descri
			END IF
		END IF

	CASE "tagr_hordes"
		IF Duplicado(ls_Columna, Data) THEN
			This.Object.tagr_hordes[il_Fila]	=	Time(ls_Nula)
			RETURN 1
		END IF
	
END CHOOSE
end event

type st_2 from statictext within w_mant_spro_gruasporturno
integer x = 265
integer y = 144
integer width = 238
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_spro_gruasporturno
integer x = 1454
integer y = 144
integer width = 210
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

type em_fecha from editmask within w_mant_spro_gruasporturno
integer x = 1687
integer y = 132
integer width = 494
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
long calendartextcolor = 8388608
long calendartitletextcolor = 32896
long calendartrailingtextcolor = 10789024
long calendarbackcolor = 16777215
long calendartitlebackcolor = 12632256
end type

event modified;id_FechaInicio	=	Date(This.Text)
end event

type uo_selplantas from uo_seleccion_plantas within w_mant_spro_gruasporturno
event destroy ( )
integer x = 489
integer y = 132
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

