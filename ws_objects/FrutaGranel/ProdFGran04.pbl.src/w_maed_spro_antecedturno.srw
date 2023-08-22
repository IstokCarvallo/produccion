$PBExportHeader$w_maed_spro_antecedturno.srw
$PBExportComments$Mantenedor que registra los antecedentes del turno, detalle de costos y detalle de incentivos diarios.
forward
global type w_maed_spro_antecedturno from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_spro_antecedturno
end type
type tp_1 from userobject within tab_1
end type
type dw_personal from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_personal dw_personal
end type
type tp_2 from userobject within tab_1
end type
type dw_incentivo from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_incentivo dw_incentivo
end type
type tab_1 from tab within w_maed_spro_antecedturno
tp_1 tp_1
tp_2 tp_2
end type
end forward

global type w_maed_spro_antecedturno from w_mant_encab_deta_csd
integer width = 3589
integer height = 2048
string title = "ANTECEDENTES DEL TURNO"
string menuname = ""
event ue_imprimir ( )
tab_1 tab_1
end type
global w_maed_spro_antecedturno w_maed_spro_antecedturno

type variables
uo_plantadesp				iuo_PltaDestino
uo_especie					iuo_Especie
uo_Variedades				iuo_Variedad
uo_lineapacking			iuo_lineapacking
uo_spro_cargospacking	iuo_cargospacking
uo_fechamovto				iuo_fechamovto

Boolean							ib_Modifica, ib_AutoCommit
DataWindowChild   			idwc_LineaPacking, idwc_Cargo, idwc_Variedad, idwc_especie,idwc_planta
DataWindow						dw_3, dw_4
str_envase						istr_envase

DataStore						ids_Base
end variables

forward prototypes
public function boolean duplicadoenvase (string as_columna, string as_valor)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaencab (boolean habilita)
public function boolean duplicadocargo (string as_columna, string as_valor)
public subroutine calcula_totalcosto (string as_columna, string as_valor)
public subroutine captura_totales ()
public subroutine generadetalle ()
public function boolean existeencabezado (string as_columna, string as_valor)
public subroutine habilitaingreso (string as_columna)
end prototypes

event ue_imprimir();
SetPointer(HourGlass!)
Long		fila
Date		ld_Fecha
str_info	lstr_info

lstr_info.titulo	= "ANTECEDENTE DE TURNO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_antecedentesturno_enca"

vinf.dw_1.SetTransObject(sqlca)

ld_Fecha	=	Date(istr_Mant.Argumento[6])

fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  ld_Fecha, &
								  Integer(istr_Mant.Argumento[3]), &
								  Integer(istr_Mant.Argumento[4]), &
								  Integer(istr_Mant.Argumento[5]))
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila =0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

public function boolean duplicadoenvase (string as_columna, string as_valor);Long		ll_Fila
String	ls_TipoEnvase, ls_CodEnvase

ls_TipoEnvase	=	String(dw_4.Object.enva_tipoen[il_Fila])
ls_CodEnvase	=	String(dw_4.Object.enva_codigo[il_Fila])

CHOOSE CASE as_Columna
	CASE "enva_tipoen"
		ls_TipoEnvase	=	as_Valor

	CASE "enva_codigo"
		ls_CodEnvase	=	as_Valor

END CHOOSE

ll_Fila	=	dw_4.Find("enva_tipoen = " + ls_TipoEnvase + " AND " + &
							"enva_codigo = " + ls_CodEnvase , &
							1, dw_4.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean			lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Borrando THEN
	IF dw_4.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_2.Update(True, False) = 1 THEN
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
			
						dw_2.ResetUpdate()
						dw_3.ResetUpdate()
						dw_4.ResetUpdate()
					END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
		
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
	
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
			
						dw_2.ResetUpdate()
						dw_3.ResetUpdate()
						dw_4.ResetUpdate()
					END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
		
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
	
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.line_codigo.Protect				=	0
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.espe_codigo.Protect				=	0
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.Protect				=	0
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.antu_numero.Protect				=	0
	dw_2.Object.antu_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.antu_turno.Protect				=	0
	dw_2.Object.antu_turno.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.antu_fecpro.Protect				=	0
	dw_2.Object.antu_fecpro.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.Object.line_codigo.Protect				=	1
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.antu_numero.Protect				=	1
	dw_2.Object.antu_numero.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.antu_turno.Protect				=	1
	dw_2.Object.antu_turno.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.antu_fecpro.Protect				=	1
	dw_2.Object.antu_fecpro.BackGround.Color	=	RGB(192,192,192)
END IF
end subroutine

public function boolean duplicadocargo (string as_columna, string as_valor);Long		ll_Fila
String	ls_TipoCargo, ls_CodCargo

ls_TipoCargo	=	String(dw_3.Object.carg_tipcar[il_Fila])
ls_CodCargo		=	String(dw_3.Object.carg_codigo[il_Fila])

CHOOSE CASE as_Columna
	CASE "carg_tipcar"
		ls_TipoCargo	=	as_Valor

	CASE "carg_codigo"
		ls_CodCargo		=	as_Valor

END CHOOSE

ll_Fila	=	dw_3.Find("carg_tipcar = " + ls_TipoCargo + " AND " + &
							  "carg_codigo = " + ls_CodCargo , &
							1, dw_3.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Cargo ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine calcula_totalcosto (string as_columna, string as_valor);Decimal{2}	ld_ValUni, ld_Numper

ld_Numper	=	dw_3.Object.ccat_numper[il_Fila]
ld_ValUni	=	dw_3.Object.ccat_valuni[il_Fila]

CHOOSE CASE as_Columna
	CASE "ccat_numper"
		ld_Numper	=	Dec(as_Valor)

	CASE "ccat_valuni"
		ld_ValUni	=	Dec(as_Valor)

END CHOOSE

dw_3.Object.ccat_valtot[il_Fila] = Round(ld_Numper * ld_Valuni,0)
end subroutine

public subroutine captura_totales ();Long			ll_Fila,ll_Total_Fijo,ll_Total_Variable, ll_Total_Incentivo

dw_3.accepttext()
dw_4.accepttext()

ll_Fila	=	dw_3.RowCount()

IF ll_Fila > 0 THEN
	ll_Total_Fijo		=	dw_3.Object.total_fijo[ll_Fila]
	ll_Total_Variable	=	dw_3.Object.total_variable[ll_Fila]
END IF

ll_Fila	=	dw_4.RowCount()

IF ll_Fila > 0 THEN
	ll_Total_Incentivo	=	dw_4.Object.total_incentivo[ll_Fila]
END IF

dw_2.Object.antu_cosper[1]	=	ll_Total_Fijo
dw_2.Object.antu_copeva[1]	=	ll_Total_Variable
dw_2.Object.antu_cosinc[1]	=	ll_Total_Incentivo

RETURN
end subroutine

public subroutine generadetalle ();Long		ll_Fila, ll_Filas, ll_FilaNueva
Integer	li_Null

SetNull(li_Null)

//	Llena Incentivos Diarios
IF dw_4.RowCount() = 0 THEN
	ids_Base.DataObject	=	"dw_mues_calibresenvase"
	ids_Base.SetTransObject(sqlca)
	
	ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[3]),0,0)
	
	IF ll_Filas = 0 THEN
		MessageBox("Atención", "No se han creado Calibres de Envase~r" + &
						"para Especie indicada.~r~rRegistre los antecedentes " + &
						"y vuelva a ingresar la información.")
	ELSE
		FOR ll_Filas = 1 TO ids_Base.RowCount()

			ll_Fila	=	dw_4.Find("enva_tipoen = " + String(ids_Base.Object.enva_tipoen[ll_Filas]) + " AND " + &
										 "enva_codigo = " + String(ids_Base.Object.enva_codigo[ll_Filas]) , &
										 1, dw_4.RowCount())

			IF ll_Fila = 0 THEN
				ll_FilaNueva	=	dw_4.InsertRow(0)
	
				
				dw_4.Object.enva_tipoen[ll_FilaNueva]	=	ids_Base.Object.enva_tipoen[ll_Filas]
				dw_4.Object.enva_codigo[ll_FilaNueva]	=	ids_Base.Object.enva_codigo[ll_Filas]
				dw_4.Object.enva_nombre[ll_FilaNueva]	=	ids_Base.Object.enva_nombre[ll_Filas]
			END IF
		NEXT
	END IF
END IF
end subroutine

public function boolean existeencabezado (string as_columna, string as_valor);Boolean	lb_Retorno
Integer	li_Planta, li_Linea, li_Especie, li_Variedad, li_Turno, li_Cantidad
Date		ld_Fecha

li_Planta	= dw_2.Object.plde_codigo[1]
li_Linea		= dw_2.Object.line_codigo[1]
li_Especie	= dw_2.Object.espe_codigo[1]
li_Variedad	= dw_2.Object.vari_codigo[1]
li_Turno		= dw_2.Object.antu_turno[1]
ld_Fecha		= dw_2.Object.antu_fecpro[1]

CHOOSE CASE as_Columna
	CASE "line_codigo"
		li_Linea		=	Integer(as_Valor)

	CASE "espe_codigo"
		li_Especie	=	Integer(as_Valor)

	CASE "vari_codigo"
		li_Variedad	=	Integer(as_Valor)

	CASE "antu_turno"
		li_Turno		=	Integer(as_Valor)

	CASE "antu_fecpro"
		ld_Fecha		=	Date(as_Valor)

END CHOOSE

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dba.spro_antecedturno
	WHERE	plde_codigo	=	:li_Planta
	AND	line_codigo	=	:li_Linea
	AND	espe_codigo	=	:li_Especie 
	AND 	vari_codigo =	:li_Variedad
	AND	antu_turno	=	:li_Turno
	AND	antu_fecpro	=	:ld_Fecha;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Antecedentes de Turno")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode <> 100 and li_Cantidad > 0 THEN
	istr_Mant.Argumento[2]	=	String(li_Linea)
	istr_Mant.Argumento[3]	=	String(li_Especie)
	istr_Mant.Argumento[4]	=	String(li_Variedad)
	istr_Mant.Argumento[5]	=	String(li_Turno)
	istr_Mant.Argumento[6]	=	String(ld_Fecha)

	This.TriggerEvent("ue_recuperadatos")

	lb_Retorno	=	True
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha

IF as_Columna <> "line_codigo" AND &
	(dw_2.Object.line_codigo[1] = 0 OR IsNull(dw_2.Object.line_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "espe_codigo" AND &
	(dw_2.Object.espe_codigo[1] = 0 OR IsNull(dw_2.Object.espe_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "vari_codigo" AND &
	(dw_2.Object.vari_codigo[1] = 0 OR IsNull(dw_2.Object.vari_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "antu_numero" AND &
	(dw_2.Object.antu_numero[1] = 0 OR IsNull(dw_2.Object.antu_numero[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "antu_turno" AND &
	(dw_2.Object.antu_turno[1] = 0 OR IsNull(dw_2.Object.antu_turno[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "antu_fecpro" AND &
	(dw_2.Object.antu_fecpro[1] = ld_Fecha OR IsNull(dw_2.Object.antu_fecpro[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "expo_codigo" AND &
	(dw_2.Object.expo_codigo[1] = 0 OR IsNull(dw_2.Object.expo_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "antu_tipdoc" AND &
	(dw_2.Object.antu_tipdoc[1] = 0 OR IsNull(dw_2.Object.antu_tipdoc[1])) THEN
	lb_Estado = False
END IF

tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Numero de Linea
istr_Mant.Argumento[3]	=	Código de Especie
istr_Mant.Argumento[4]	=	Código de Variedad
istr_Mant.Argumento[5]	=	Código de Turno
istr_Mant.Argumento[6]	=	Fecha de Proceso
istr_Mant.Argumento[7]	=	Número
*/

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_3	=	tab_1.tp_1.dw_personal
dw_4	=	tab_1.tp_2.dw_incentivo

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	""
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	""
istr_Mant.Argumento[5]	=	""
istr_Mant.Argumento[6]	=	""

dw_2.GetChild("line_codigo", idwc_LineaPacking)
idwc_LineaPacking.SetTransObject(sqlca)
idwc_LineaPacking.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	idwc_Planta.InsertRow(0)
END IF

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especie.InsertRow(0)
END IF

dw_2.GetChild("vari_codigo", idwc_Variedad)
idwc_Variedad.SetTransObject(sqlca)
idwc_Variedad.InsertRow(0)

dw_3.GetChild("carg_codigo", idwc_Cargo)
idwc_Cargo.SetTransObject(SqlCa)
idwc_Cargo.Retrieve(1)
idwc_Cargo.Reset()
idwc_Cargo.InsertRow(0)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 88")

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.Modify("DataWindow.Footer.Height = 88")

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_PltaDestino	=	Create uo_plantadesp
iuo_LineaPacking	=	Create uo_lineapacking
iuo_Especie			=	Create uo_Especie
iuo_CargosPacking	=	Create uo_spro_cargospacking
iuo_Variedad		=	Create uo_Variedades
iuo_FechaMovto		=	Create uo_FechaMovto

ids_Base				=	Create DataStore
end event

event ue_borra_detalle();call super::ue_borra_detalle;IF dw_3.RowCount() < 1 THEN 
	RETURN
ELSE
	pb_ins_det.Enabled	=	True
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_3.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_3.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_3.GetRow()
	END IF
END IF
end event

event ue_nuevo_detalle();call super::ue_nuevo_detalle;IF tab_1.SelectedTab = 1 THEN
	dw_3.SetColumn("carg_tipcar")

	il_fila = dw_3.InsertRow(0)

	IF il_fila > 0 THEN
		pb_eliminar.Enabled	= True
		pb_eli_det.Enabled	= TRUE
		pb_grabar.Enabled		= True
	END IF
	
	dw_3.ScrollToRow(il_fila)
	dw_3.SetRow(il_fila)
	dw_3.SetFocus()
	dw_3.SetColumn(1)
END IF

IF dw_3.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_3.RowCount() > 0 THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta
Date		ld_Fecha

ld_Fecha	=	Date(Mid(istr_Mant.Argumento[6], 1, 10))

DO
	dw_2.SetRedraw(False)

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
										  Integer(istr_Mant.Argumento[3]), &
										  Integer(istr_Mant.Argumento[4]), &
										  Integer(istr_Mant.Argumento[5]), &
 										  ld_Fecha)
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			IF dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Integer(istr_Mant.Argumento[3]), &
								  Integer(istr_Mant.Argumento[4]), &
								  Integer(istr_Mant.Argumento[5]), &
  								  ld_Fecha) = -1 OR &
				dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Integer(istr_Mant.Argumento[3]), &
								  Integer(istr_Mant.Argumento[4]), &
								  Integer(istr_Mant.Argumento[5]), &
								  ld_Fecha) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
												"No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				dw_2.GetChild("vari_codigo", idwc_Variedad)
				idwc_Variedad.SetTransObject(sqlca)
				idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
				
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True

				HabilitaEncab(False)
				
				tab_1.tp_1.Enabled	=	TRUE
				tab_1.tp_2.Enabled	=	TRUE
				
				pb_eli_det.Enabled	=	True
				
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				dw_1.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_spro_antecedturno.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_spro_antecedturno.destroy
call super::destroy
destroy(this.tab_1)
end on

event ue_nuevo();call super::ue_nuevo;Date	ld_Fecha

ld_Fecha	=	date(F_FechaHora())

dw_3.Reset()
dw_4.Reset()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.antu_fecpro[1]	=	Date(ld_Fecha)
istr_Mant.Argumento[6]	=	string(ld_Fecha)
end event

event ue_antesguardar();call super::ue_antesguardar;Long		ll_Fila
Date 		ld_fecha
Time		lt_Hora
Integer	li_cont
String	ls_mensaje, ls_colu[]

Message.DoubleParm = 1

IF Isnull(dw_2.Object.expo_codigo[1]) OR dw_2.Object.expo_codigo[1] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEmpresa"
	ls_colu[li_cont]	= "expo_codigo"
END IF

IF Isnull(dw_2.Object.antu_horini[1]) OR dw_2.Object.antu_horini[1] = lt_Hora THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nHora de Inicio de Horario"
	ls_colu[li_cont]	= "antu_horini"
END IF

IF Isnull(dw_2.Object.antu_horter[1]) OR dw_2.Object.antu_horter[1] = lt_Hora THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nHora de Término de Horario"
	ls_colu[li_cont]	= "antu_horter"
END IF

IF Isnull(dw_2.Object.antu_finilab[1]) OR dw_2.Object.antu_finilab[1] = ld_fecha THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha de Inicio de Horario"
	ls_colu[li_cont]	= "antu_finilab"
END IF

IF Isnull(dw_2.Object.antu_fterlab[1]) OR dw_2.Object.antu_fterlab[1] = ld_fecha THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFecha de Término de Horario"
	ls_colu[li_cont]	= "antu_fterlab"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_colu[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF

IF dw_2.Object.antu_horini[1] > dw_2.Object.antu_horter[1] THEN
	Messagebox("Error de Consistencia","El horario de inicio debe ser menor al horario de término.")
	Message.DoubleParm = -1
	RETURN
END IF

IF dw_2.Object.antu_hoexin[1] > dw_2.Object.antu_hoexte[1] THEN
	Messagebox("Error de Consistencia","El horas extras inicio debe ser menor a horas extras término.")
	Message.DoubleParm = -1
	RETURN
END IF

FOR ll_Fila	= 1 TO dw_3.RowCount()

	IF isnull(dw_3.Object.carg_tipcar[ll_fila]) OR  dw_3.Object.carg_tipcar[ll_fila]=0 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " de Personal Diario debe Ingresar Tipo de Cargo.")
		Message.DoubleParm = -1
		RETURN
	END IF
	
	IF isnull(dw_3.Object.carg_codigo[ll_fila]) OR  dw_3.Object.carg_codigo[ll_fila]=0 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " de Personal Diario debe Ingresar un Cargo.")
		Message.DoubleParm = -1
		RETURN
	END IF

	IF dw_3.Object.ccat_numper[ll_fila] < 0 OR dw_3.Object.ccat_numper[ll_fila] > 9999.99 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " el número de" + &
														"~r~rpersonas debe ser mayor que 0 y menor que 9.999,99")
		Message.DoubleParm = -1
		RETURN
	END IF

	IF dw_3.Object.ccat_valuni[ll_fila] < 0 OR dw_3.Object.ccat_valuni[ll_fila] > 9999999.99 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " de Personal Diario el valor unitario" + &
														"~r~rdebe ser mayor que 0 y menor que 9.999.999,99")
		Message.DoubleParm = -1
		RETURN
	END IF
	
	IF dw_3.Object.ccat_valtot[ll_fila] < 0 OR dw_3.Object.ccat_valtot[ll_fila] > 999999999 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " de Personal Diario el valor total" + &
														"~r~rdebe ser mayor que 0 y menor que 999.999.999")
		Message.DoubleParm = -1
		RETURN
	END IF
	
NEXT

FOR ll_Fila	= 1 TO dw_4.RowCount()

	IF isnull(dw_4.Object.enva_tipoen[ll_fila]) OR  dw_4.Object.enva_tipoen[ll_fila]=0 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " de Incentivos Diario debe Ingresar Tipo de Envase.")
		Message.DoubleParm = -1
		RETURN
	END IF
	
	IF isnull(dw_4.Object.enva_codigo[ll_fila]) OR  dw_4.Object.enva_codigo[ll_fila]=0 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " de Incentivos Diario debe Ingresar un Envase.")
		Message.DoubleParm = -1
		RETURN
	END IF

	IF dw_4.Object.dinc_cancaj[ll_fila] < 0 OR dw_4.Object.dinc_cancaj[ll_fila] > 9999999 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " de Incentivos Diario la cantidad de" + &
														"~r~rcajas debe ser mayor que 0 y menor que 9.999.999")
		Message.DoubleParm = -1
		RETURN
	END IF

	IF dw_4.Object.dinc_valuni[ll_fila] < 0 OR dw_4.Object.dinc_valuni[ll_fila] > 9999.99 THEN
		Messagebox("Error de Consistencia","En la fila " +String(ll_fila)+ " de Incentivos Diario el valor unitario" + &
														"~r~rdebe ser mayor que 0 y menor que 9.999,99")
		Message.DoubleParm = -1
		RETURN
	END IF
	
NEXT
	
	
FOR ll_Fila = 1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_3.Object.line_codigo[ll_Fila]	=	dw_2.Object.line_codigo[1]
		dw_3.Object.espe_codigo[ll_Fila]	=	dw_2.Object.espe_codigo[1]
		dw_3.Object.vari_codigo[ll_Fila]	=	dw_2.Object.vari_codigo[1]
		dw_3.Object.antu_turno[ll_Fila]	=	dw_2.Object.antu_turno[1]
		dw_3.Object.antu_fecpro[ll_Fila]	=	dw_2.Object.antu_fecpro[1]
	END IF
NEXT

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_4.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_4.Object.line_codigo[ll_Fila]	=	dw_2.Object.line_codigo[1]
		dw_4.Object.espe_codigo[ll_Fila]	=	dw_2.Object.espe_codigo[1]
		dw_4.Object.vari_codigo[ll_Fila]	=	dw_2.Object.vari_codigo[1]
		dw_4.Object.antu_turno[ll_Fila]	=	dw_2.Object.antu_turno[1]
		dw_4.Object.antu_fecpro[ll_Fila]	=	dw_2.Object.antu_fecpro[1]
	END IF
NEXT
end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN
	dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
END IF

IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF

IF dw_2.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		
		w_main.SetMicroHelp("Borrando Registro...")
		
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_Borrar	=	False
	
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event resize;call super::resize;tab_1.x			=	dw_1.x
tab_1.y			=	dw_1.y
tab_1.Height	=	dw_1.Height

end event

event ue_seleccion();call super::ue_seleccion;Str_Busqueda	lstr_busq

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]
lstr_Busq.Argum[2]	=	istr_Mant.Argumento[3]
lstr_Busq.Argum[3]	=	istr_Mant.Argumento[2]

OpenWithParm(w_busc_spro_antecedturno, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[5] <> "" THEN
	istr_Mant.Argumento[1]	=	lstr_Busq.Argum[1]
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	istr_Mant.Argumento[4]	=	lstr_Busq.Argum[4]
	istr_Mant.Argumento[5]	=	lstr_Busq.Argum[5]
	istr_Mant.Argumento[6]	=	lstr_Busq.Argum[6]

	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_validaborrar();//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_antecedturno
boolean visible = false
integer x = 37
integer y = 1084
integer width = 2990
integer height = 708
boolean enabled = false
string title = ""
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_antecedturno
integer x = 23
integer y = 36
integer width = 2862
integer height = 816
integer taborder = 10
string dataobject = "dw_mant_spro_antecedturno"
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna
Date		ld_Fecha, ld_Fecha1

ls_Columna	=	dwo.Name
SetNull(ls_Nula)

CHOOSE CASE ls_Columna

	CASE "line_codigo"
		IF Not iuo_LineaPacking.Existe(gstr_ParamPlanta.CodigoPlanta, Integer(Data), True, sqlca) AND &
			Not ExisteEncabezado(ls_Columna, Data) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))

			RETURN 1
		ELSE
			istr_Mant.Argumento[2] = Data
		END IF

	CASE "espe_codigo"
		IF Not iuo_Especie.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))

			RETURN 1
		ELSE
			istr_Mant.Argumento[3]	=	Data
			This.Object.vari_codigo[1]	=	Integer(ls_Nula)
			
			This.GetChild("vari_codigo", idwc_Variedad)
			idwc_Variedad.SetTransObject(sqlca)
			idwc_Variedad.Retrieve(Integer(data))
			
			IF NOT ExisteEncabezado(ls_Columna, Data) THEN
				GeneraDetalle()
			END IF
		END IF
		
	CASE "vari_codigo"
		IF Not iuo_Variedad.Existe(This.Object.espe_codigo[row], Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))

			RETURN 1
		ELSE
			istr_Mant.Argumento[4]	=	Data
			ExisteEncabezado(ls_Columna, Data)
		END IF

	CASE "antu_turno"
		istr_Mant.Argumento[5]	=	Data
		ExisteEncabezado(ls_Columna, Data)

	CASE "antu_fecpro"
		istr_Mant.Argumento[6]	=	Data
		ExisteEncabezado(ls_Columna, Data)

	CASE "antu_numero"
		istr_Mant.Argumento[7]	=	Data
		
	CASE "antu_finilab", "antu_finiext"
		IF NOT iuo_FechaMovto.Valida_FechaMovto(Date(data)) THEN
			This.SetItem(1, ls_Columna, Date(ls_Nula))
			
			RETURN 1
		END IF
		
	CASE "antu_fterlab"
		ld_Fecha		=	dw_2.Object.antu_finilab[row]
		ld_Fecha1	=	Date(data)
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha1) THEN
			This.SetItem(1, ls_Columna, Date(ls_Nula))			
			RETURN 1
		ELSEIF ld_Fecha > ld_Fecha1 THEN
			MessageBox("Error","Fecha de Termino es Inferior a la de Inicio",Exclamation!)
			This.SetItem(1, ls_Columna, Date(ls_Nula))
			
			RETURN 1
		END IF
		
	CASE "antu_fterext"
		ld_Fecha	=	dw_2.Object.antu_finiext[row]
		ld_Fecha1	=	Date(data)
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha1) THEN
			This.SetItem(1, ls_Columna, Date(ls_Nula))
			
			RETURN 1
		ELSEIF ld_Fecha > ld_Fecha1 THEN
			MessageBox("Error","Fecha de Termino es Inferior a la de Inicio",Exclamation!)
			This.SetItem(1, ls_Columna, Date(ls_Nula))
			
			RETURN 1
		END IF

//ELSEIF ld_Fecha >= ld_Fecha1 THEN
//			MessageBox("Error","La Hora de Término es Inferior a la de Inicio",Exclamation!)
//			This.SetItem(1, ls_Columna, Date(ls_Nula))
//			
//			RETURN 1
//		END IF
//	CASE "antu_horini", "antu_hoexin"
//				IF ld_Fecha >= ld_Fecha1 THEN
//			MessageBox("Error","La Hora de Termino es Inferior a la de Inicio",Exclamation!)
//			This.SetItem(1, ls_Columna, Date(ls_Nula))
//			
//			RETURN 1
//		END IF
//		
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_antecedturno
integer x = 3287
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_antecedturno
integer x = 3287
integer taborder = 70
end type

event pb_eliminar::clicked;call super::clicked;//str_Mant		lstr_Mant
//
//lstr_Mant.Argumento[1]	=	"1"
//lstr_Mant.Argumento[2]	=	istr_Mant.Argumento[1]
//lstr_Mant.Argumento[3]	=	istr_Mant.Argumento[2]
//lstr_Mant.Argumento[4]	=	istr_Mant.Argumento[3]
//lstr_Mant.Argumento[5]	=	"M"
//
//OpenWithParm(w_mant_bitacorasitua, lstr_Mant)
end event

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_antecedturno
integer x = 3287
integer y = 632
integer taborder = 80
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_antecedturno
integer x = 3287
integer taborder = 90
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_antecedturno
integer x = 3287
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_antecedturno
integer x = 3287
integer y = 1504
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_antecedturno
integer x = 3287
integer y = 1680
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_antecedturno
integer x = 3287
integer taborder = 50
end type

type tab_1 from tab within w_maed_spro_antecedturno
event create ( )
event destroy ( )
integer x = 32
integer y = 912
integer width = 2990
integer height = 972
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.Control[]={this.tp_1,&
this.tp_2}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
end on

event selectionchanged;IF NewIndex = 1 THEN
	IF dw_3.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		pb_ins_det.Enabled	=	True
		il_Fila 					=	1
		
	ELSE
		pb_eli_det.Enabled	=	False
		pb_ins_det.Enabled	=	True
	END IF
ELSE
	pb_eli_det.Enabled	=	False
	pb_ins_det.Enabled	=	False
	il_Fila 					=	1
END IF
end event

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 2953
integer height = 844
boolean enabled = false
long backcolor = 12632256
string text = "Personal Diario"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Table!"
long picturemaskcolor = 553648127
dw_personal dw_personal
end type

on tp_1.create
this.dw_personal=create dw_personal
this.Control[]={this.dw_personal}
end on

on tp_1.destroy
destroy(this.dw_personal)
end on

type dw_personal from uo_dw within tp_1
integer x = 37
integer y = 36
integer width = 2853
integer height = 780
integer taborder = 11
string dataobject = "dw_mues_spro_costocargodeta"
boolean hscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
	dw_3.GetChild("carg_codigo", idwc_Cargo)
	idwc_Cargo.SetTransObject(SqlCa)
	idwc_Cargo.Retrieve(dw_3.Object.carg_tipcar[il_fila])
END IF
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event itemchanged;call super::itemchanged;String		ls_columna, ls_Null

SetNull(ls_Null)

ls_Columna = GetColumnName()

CHOOSE CASE ls_Columna

	CASE "carg_tipcar"
		istr_Mant.Argumento[7]	=	Data
		dw_3.GetChild("carg_codigo", idwc_Cargo)
		idwc_Cargo.SetTransObject(SqlCa)
		idwc_Cargo.Retrieve(Integer(Data))
		This.SetItem(il_Fila,"carg_codigo",Integer(ls_Null))

	CASE "carg_codigo"
		IF NOT iuo_CargosPacking.Existe(Integer(istr_Mant.Argumento[7]), &
		   Integer(Data), True, Sqlca) OR DuplicadoCargo(ls_Columna,Data) THEN
			This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			This.Object.carg_nombre[il_fila] =	iuo_CargosPacking.NombreCargo
			This.Object.ccat_valuni[il_Fila]	=	iuo_CargosPacking.Costo
			Calcula_TotalCosto(ls_Columna, "0")
		END IF

	CASE "ccat_numper"
		Calcula_TotalCosto(ls_Columna, Data)

	CASE "ccat_valuni"
		Calcula_TotalCosto(ls_Columna, Data)
													   
END CHOOSE

Captura_Totales()
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 2953
integer height = 844
boolean enabled = false
long backcolor = 12632256
string text = "Incentivos Diarios"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeTables!"
long picturemaskcolor = 553648127
dw_incentivo dw_incentivo
end type

on tp_2.create
this.dw_incentivo=create dw_incentivo
this.Control[]={this.dw_incentivo}
end on

on tp_2.destroy
destroy(this.dw_incentivo)
end on

type dw_incentivo from uo_dw within tp_2
integer x = 37
integer y = 36
integer width = 2853
integer height = 780
integer taborder = 10
string dataobject = "dw_mues_spro_detincentivos"
boolean hscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event itemchanged;call super::itemchanged;String  ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna

	CASE "enva_tipoen"
		IF NOT ExisteEnvase(Integer(Data), 0, istr_Envase) OR &
			DuplicadoEnvase(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))

			RETURN 1
		END IF

	CASE "enva_codigo"
		IF NOT ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) OR &
			DuplicadoEnvase(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			This.SetItem(il_Fila, "enva_nombre", ls_Nula)

			RETURN 1
		ELSE
			This.Object.enva_nombre[il_Fila]	=	istr_Envase.Nombre
		END IF

END CHOOSE

Captura_Totales()
end event

