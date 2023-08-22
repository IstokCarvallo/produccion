$PBExportHeader$w_maed_duchacontrol_cierre.srw
$PBExportComments$Cierre de Duchas
forward
global type w_maed_duchacontrol_cierre from w_mant_encab_deta_csd
end type
end forward

global type w_maed_duchacontrol_cierre from w_mant_encab_deta_csd
integer width = 3227
integer height = 1964
string title = "CIERRE DE DUCHA"
string menuname = ""
event ue_validaregistro ( )
end type
global w_maed_duchacontrol_cierre w_maed_duchacontrol_cierre

type variables
w_mant_detalle	iw_mantencion

Integer	ii_seleccion, il_filla
Date		id_fecini
Time 		it_horini

uo_duchacontrol		iuo_duchacontrol
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades

DataWindowChild	idwc_especie, idwc_grupo, idwc_subgrupo, idwc_variedad, &
						idwc_especiealt, idwc_grupoalt, idwc_subgrupoalt, idwc_variedadalt
end variables

forward prototypes
public subroutine habilita_encab (boolean habilita)
public subroutine habilitatipoingreso (integer ai_tipoingreso)
public function boolean existeencabezado (string as_columna, string as_valor)
public subroutine chequeatipoingreso ()
end prototypes

public subroutine habilita_encab (boolean habilita);IF Habilita THEN
	dw_2.Object.duch_codigo.Protect				=	0
	dw_2.Object.codu_nropos.Protect				=	0
	dw_2.Object.codu_fecini.Protect				=	0
	dw_2.Object.codu_horini.Protect				=	0
	dw_2.Object.duch_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_nropos.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_fecini.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_horini.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.Object.duch_codigo.Protect				=	1
	dw_2.Object.codu_nropos.Protect				=	1
	dw_2.Object.codu_fecini.Protect				=	1
	dw_2.Object.codu_horini.Protect				=	1
	dw_2.Object.duch_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_nropos.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_fecini.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_horini.BackGround.Color	=	RGB(192,192,192)
END IF
end subroutine

public subroutine habilitatipoingreso (integer ai_tipoingreso);
CHOOSE CASE ai_TipoIngreso

	CASE 1 // Por Especie
		dw_2.Object.grva_codigo.Visible	=	False
		dw_2.Object.grva_codalt.Visible	=	False
		dw_2.Object.grva_codsub.Visible	=	False
		dw_2.Object.grva_subalt.Visible	=	False		
		dw_2.Object.vari_codigo.Visible	=	False
		dw_2.Object.vari_codalt.Visible	=	False
		dw_2.Object.t_grupo.Visible		=	False
		dw_2.Object.t_grupoalt.Visible	=	False
		dw_2.Object.t_subgrupo.Visible	=	False
		dw_2.Object.t_subgrupoalt.Visible=	False		
		dw_2.Object.t_variedad.Visible	=	False
		dw_2.Object.t_variedadalt.Visible=	False

	CASE 2 // Por Grupo
		dw_2.Object.grva_codigo.Visible	=	True
		dw_2.Object.grva_codalt.Visible	=	True		
		dw_2.Object.grva_codsub.Visible	=	False
		dw_2.Object.grva_subalt.Visible	=	False	
		dw_2.Object.vari_codigo.Visible	=	False
		dw_2.Object.vari_codalt.Visible	=	False		
		dw_2.Object.t_grupo.Visible		=	True
		dw_2.Object.t_grupoalt.Visible	=	True		
		dw_2.Object.t_subgrupo.Visible	=	False
		dw_2.Object.t_subgrupoalt.Visible=	False		
		dw_2.Object.t_variedad.Visible	=	False
		dw_2.Object.t_variedadalt.Visible=	False

	CASE 3 // Por Sub Grupo
		dw_2.Object.grva_codigo.Visible	=	True
		dw_2.Object.grva_codalt.Visible	=	True
		dw_2.Object.grva_codsub.Visible	=	True
		dw_2.Object.grva_subalt.Visible	=	True
		dw_2.Object.vari_codigo.Visible	=	False
		dw_2.Object.vari_codalt.Visible	=	False		
		dw_2.Object.t_grupo.Visible		=	True
		dw_2.Object.t_grupoalt.Visible	=	True		
		dw_2.Object.t_subgrupo.Visible	=	True
		dw_2.Object.t_subgrupoalt.Visible=	True		
		dw_2.Object.t_variedad.Visible	=	False
		dw_2.Object.t_variedadalt.Visible=	False		

	CASE 4 // Por Variedad
		dw_2.Object.vari_codigo.Visible	=	True
		dw_2.Object.vari_codalt.Visible	=	True		
		dw_2.Object.grva_codigo.Visible	=	False
		dw_2.Object.grva_codalt.Visible	=	False		
		dw_2.Object.grva_codsub.Visible	=	False
		dw_2.Object.grva_subalt.Visible	=	False
		dw_2.Object.t_variedad.Visible	=	True
		dw_2.Object.t_variedadalt.Visible=	True		
		dw_2.Object.t_grupo.Visible		=	False
		dw_2.Object.t_grupoalt.Visible	=	False		
		dw_2.Object.t_subgrupo.Visible	=	False
		dw_2.Object.t_subgrupoalt.Visible	=	False		

END CHOOSE
end subroutine

public function boolean existeencabezado (string as_columna, string as_valor);Boolean	lb_Retorno
Integer	li_Ducha, li_Estanque, li_Cantidad
Date		ld_Fecha,ld_FechaNula
Time		lt_Hora

li_Ducha		= dw_2.Object.duch_codigo[1]
li_Estanque	= dw_2.Object.codu_nropos[1]
ld_Fecha	= dw_2.Object.codu_fecini[1]
lt_Hora		= dw_2.Object.codu_horini[1]

CHOOSE CASE as_Columna
	CASE "duch_codigo"
		li_Ducha		=	Integer(as_Valor)
		
	CASE "codu_nropos"
		li_Estanque	=	Integer(as_Valor)
		
	CASE "codu_fecini"
		ld_Fecha	=	Date(Mid(as_Valor, 1, 10))
		
	CASE "codu_horini"
		lt_Hora		=	Time(Mid(as_Valor, 12))
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dba.spro_duchacontrol
	WHERE	duch_codigo	=	:li_Ducha
	AND	codu_nropos	=	:li_Estanque
	AND	codu_fecini	=	:ld_Fecha
	AND	codu_horini	=	:lt_Hora ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de spro_duchacontrol")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode <> 100 AND li_cantidad>0 THEN
	istr_mant.argumento[1]	=	String(li_Ducha)
	istr_mant.argumento[2]	=	String(li_Estanque)
	istr_mant.argumento[3]	=	String(ld_Fecha)
	istr_mant.argumento[4]	=	String(lt_Hora)

	This.TriggerEvent("ue_recuperadatos")

	lb_Retorno	=	True
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine chequeatipoingreso ();IF NOT IsNull(dw_2.Object.vari_codigo[1]) THEN
	dw_2.Object.codu_tipova[1] = 4
	HabilitaTipoIngreso(4)
	RETURN
ELSEIF dw_2.Object.grva_codsub[1] > 0 THEN
	dw_2.Object.codu_tipova[1] = 3
	HabilitaTipoIngreso(3)
	RETURN
ELSEIF NOT IsNull(dw_2.Object.grva_codigo[1]) THEN
	dw_2.Object.codu_tipova[1] = 2
	HabilitaTipoIngreso(2)
	RETURN
ELSE
	dw_2.Object.codu_tipova[1] = 1
	HabilitaTipoIngreso(1)
	RETURN
END IF
end subroutine

event ue_seleccion();call super::ue_seleccion;OpenWithParm(w_busc_duchacontrol, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1] = istr_busq.argum[1]
	istr_mant.argumento[2] = istr_busq.argum[2]
	istr_mant.argumento[3] = istr_busq.argum[3]
	istr_mant.argumento[4] = istr_busq.argum[4]
	
	TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila_e, respuesta
Date		ld_Fecha, ld_Nula
Time		lt_Hora

dw_1.SettransObject(sqlca)
dw_2.SettransObject(sqlca)

ld_Fecha	=	Date(istr_mant.Argumento[3])

lt_Hora		=	Time(istr_mant.Argumento[4])

DO
	dw_2.SetRedraw(False)
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										ld_Fecha, &
										lt_Hora)
	



	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		ChequeaTipoIngreso()
		Habilita_Encab(False)
		

		IF iuo_duchacontrol.Captura_BinsDuchados(Integer(istr_Mant.Argumento[1]), &
													 		  Integer(istr_Mant.Argumento[2]), &
															  ld_Fecha, lt_Hora,SQLCA) THEN
	
			dw_2.SetItem(1,"codu_canbul",iuo_duchacontrol.ii_TotatBinsDuchados)
		END IF

		dw_2.Object.duch_ventana[1]	=	2
		pb_eliminar.Enabled				=	True
		pb_grabar.Enabled					=	True
		pb_imprimir.Enabled				=	True
		
		IF IsNull(dw_2.Object.codu_fecter[1]) OR &
			dw_2.Object.codu_fecter[1] = ld_Nula THEN
			dw_2.Object.codu_fecter[1]		=	Date(Today())
			dw_2.Object.codu_horter[1]		=	Time(Now())
		END IF
		
		dw_2.SetColumn("codu_fecter")
	END IF
LOOP WHILE respuesta = 1

dw_2.SetRedraw(True)

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
Date		ld_Fecha
Time		lt_Hora

istr_info.titulo	= "BITACORA DE PRODUCCION DUCHA BINS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_bitacora_produccion_duchabins"

vinf.dw_1.SetTransObject(sqlca)

ld_Fecha		=	Date(Mid(istr_mant.Argumento[3], 1, 10))
lt_Hora		=	Time(Mid(istr_mant.Argumento[4], 12))

fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
								  ld_Fecha, lt_Hora)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	IF iuo_Especie.Existe(dw_2.Object.espe_codigo[1],False,Sqlca) THEN
		vinf.dw_1.Modify("t_especie.Text = '" + iuo_especie.Nombre + "'")
	END IF

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

on w_maed_duchacontrol_cierre.create
call super::create
end on

on w_maed_duchacontrol_cierre.destroy
call super::destroy
end on

event open;call super::open;/*
Argumentos
	istr_mant.argumento[1]	=	Código Ducha
	istr_mant.argumento[2] 	=	Nº Pozo
	istr_mant.argumento[3]	=	Fecha Inicio Y Evento
	istr_mant.argumento[4]	=	Hora Inicio Y Evento
	istr_mant.argumento[5]	=	Tipo Evento
	istr_mant.argumento[6]	=	Cantidad Bins
*/

//This.Height			=	1888

iuo_duchacontrol		=	Create uo_duchacontrol
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades

//Especie
dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especie.InsertRow(0)
END IF

//Especie Alternativa
dw_2.GetChild("espe_codalt", idwc_especiealt)
idwc_especiealt.SetTransObject(sqlca)
IF idwc_especiealt.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especiealt.InsertRow(0)
END IF

//Grupo
dw_2.GetChild("grva_codigo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
idwc_grupo.InsertRow(0)


//Grupo Alternativo
dw_2.GetChild("grva_codalt",idwc_grupoalt)
idwc_grupoalt.SetTransObject(Sqlca)
idwc_grupoalt.InsertRow(0)

//Sub Grupo
dw_2.GetChild("grva_codsub",idwc_subgrupo)
idwc_subgrupo.SetTransObject(Sqlca)
idwc_subgrupo.InsertRow(0)

//Sub Grupo Alternativo
dw_2.GetChild("grva_subalt",idwc_subgrupoalt)
idwc_subgrupoalt.SetTransObject(Sqlca)
idwc_subgrupoalt.InsertRow(0)

//Variedad
dw_2.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.InsertRow(0)

//Variedad Alternativa
dw_2.GetChild("vari_codalt",idwc_variedadalt)
idwc_variedadalt.SetTransObject(Sqlca)
idwc_variedadalt.InsertRow(0)

dw_2.Object.codu_tipova.Protect				=	1
dw_2.Object.espe_codigo.Protect				=	1
dw_2.Object.espe_codalt.Protect				=	1
dw_2.Object.grva_codigo.Protect				=	1
dw_2.Object.grva_codalt.Protect				=	1
dw_2.Object.grva_codsub.Protect				=	1
dw_2.Object.grva_subalt.Protect				=	1
dw_2.Object.vari_codigo.Protect				=	1
dw_2.Object.vari_codalt.Protect				=	1
dw_2.Object.codu_tilaca.Protect				=	1
dw_2.Object.codu_filtro.Protect				=	1
dw_2.Object.codu_litros.Protect				=	1
dw_2.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.espe_codalt.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.grva_codigo.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.grva_codalt.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.grva_codsub.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.grva_subalt.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.vari_codigo.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.vari_codalt.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.codu_tilaca.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.codu_filtro.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.codu_litros.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.codu_tiemax.BackGround.Color	=	RGB(192,192,192)
dw_2.Object.codu_maxbul.BackGround.Color	=	RGB(192,192,192)

end event

event ue_antesguardar;Integer	li_Contador
String	ls_Mensaje, ls_Columna[]
Date		ld_FecNula
Time		lt_HorNula

 IF IsNull(dw_2.Object.codu_fecter[1]) OR dw_2.Object.codu_fecter[1] = ld_FecNula THEN
	li_Contador ++
	ls_Mensaje 					=+	"~nFecha de Cierre"
	ls_Columna[li_Contador]	=	"codu_fecter"
END IF

/*IF IsNull(dw_2.Object.codu_horter[1]) OR dw_2.Object.codu_horter[1] = lt_HorNula THEN
	li_Contador ++
	ls_Mensaje 					=+	"~Hora de Cierre"
	ls_Columna[li_Contador]	=	"codu_horter"
END IF*/

IF IsNull(dw_2.Object.codu_observ[1]) OR dw_2.Object.codu_observ[1] = "" THEN
	li_Contador ++
	ls_Mensaje 					=+	"~nObservaciones de Cierre"
	ls_Columna[li_Contador]	=	"codu_observ"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_Mensaje + &
					".", StopSign!, Ok!)
	dw_2.SetColumn(ls_Columna[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_borrar();String	ls_Nula
Date		ld_FecNula
Time		lt_HorNula

SetNull(ls_Nula)

IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

dw_2.Object.codu_fecter[1]	=	ld_FecNula
dw_2.Object.codu_horter[1]	=	lt_HorNula
dw_2.Object.codu_observ[1]	=	ls_Nula

w_main.SetMicroHelp("Borrando Registro...")

IF wf_actualiza_db(True) THEN
	w_main.SetMicroHelp("Registro Borrado...")
	
	This.TriggerEvent("ue_nuevo")
	
	SetPointer(Arrow!)
ELSE
	w_main.SetMicroHelp("Registro no Borrado...")
END IF
end event

event ue_validaborrar();//
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_nuevo();call super::ue_nuevo;Habilita_Encab(True)

dw_2.SetColumn("duch_codigo")


end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_duchacontrol_cierre
boolean visible = false
integer x = 78
integer y = 1872
integer width = 2441
integer height = 128
integer taborder = 30
boolean titlebar = false
string title = ""
boolean hscrollbar = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_duchacontrol_cierre
integer x = 78
integer y = 68
integer width = 2569
integer height = 1728
integer taborder = 10
string dataobject = "dw_mant_duchacontrol"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula
Date		ld_Fecha 
Time		lt_Hora

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "duch_codigo", "codu_nropos", "codu_fecini"
	   ExisteEncabezado(ls_Columna, Data)

	CASE "codu_horini"
	   ExisteEncabezado(ls_Columna, Data)

	CASE "codu_fecter"
		
		ld_Fecha	=	dw_2.Object.codu_fecini[row]
		IF Date(Data) < ld_Fecha THEN
			MessageBox("Error", "Fecha de Cierre no debe ser Anterior a Inicio." + &
							"~r~rIngrese otra Fecha de Cierre.")
			
			This.SetItem(1, ls_Columna, Date(ls_Nula))
			
			RETURN 1
		END IF
		
	CASE "codu_horter"
		IF dw_2.Object.codu_fecini[1] = dw_2.Object.codu_fecter[1] AND &
			Time(Data) < Time(dw_2.Object.codu_horini[1]) THEN
			MessageBox("Error", "Hora de Cierre no debe ser Anterior a Inicio." + &
							"~r~rIngrese otra Hora de Cierre.")
			
			This.SetItem(1, ls_Columna, Time(ls_Nula))
			
			RETURN 1
		END IF
		
	CASE "codu_canbul"
		IF dwo.Type = 'column' THEN
			IF Not This.uf_validate(row) THEN
				This.SetItem(row, "codu_canbul", Integer(ls_Nula))
				RETURN 1 
			END IF
		END IF
		
END CHOOSE
end event

event dw_2::constructor;call super::constructor;This.uf_add_validation('codu_canbul > 0 and codu_canbul < 99999','Valores fuera de rango para cantidad de bultos')
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_duchacontrol_cierre
integer x = 2857
integer y = 320
integer height = 124
integer taborder = 70
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_duchacontrol_cierre
integer x = 2862
integer y = 272
integer taborder = 0
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_duchacontrol_cierre
integer x = 2862
integer y = 508
integer taborder = 50
end type

event pb_grabar::clicked;call super::clicked;IF Message.DoubleParm = -1 THEN RETURN

TriggerEvent("ue_nuevo")

end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_duchacontrol_cierre
integer x = 2862
integer y = 688
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_duchacontrol_cierre
integer x = 2862
integer y = 868
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_duchacontrol_cierre
boolean visible = false
integer x = 2862
integer y = 1264
integer taborder = 20
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_duchacontrol_cierre
boolean visible = false
integer x = 2862
integer y = 1432
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_duchacontrol_cierre
integer x = 2862
integer y = 132
integer taborder = 0
end type

