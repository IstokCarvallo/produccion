$PBExportHeader$w_maed_spro_orden_proceso_directo.srw
$PBExportComments$Ventana de mantención de Orden de Proceso de Fruta Granel sin Programa de Proceso
forward
global type w_maed_spro_orden_proceso_directo from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_spro_orden_proceso_directo
end type
type dw_4 from datawindow within w_maed_spro_orden_proceso_directo
end type
type st_1 from statictext within w_maed_spro_orden_proceso_directo
end type
end forward

global type w_maed_spro_orden_proceso_directo from w_mant_encab_deta_csd
string title = "Orden de Proceso Directo"
string menuname = ""
dw_3 dw_3
dw_4 dw_4
st_1 st_1
end type
global w_maed_spro_orden_proceso_directo w_maed_spro_orden_proceso_directo

type variables
uo_especie 			 	iuo_especie
uo_variedades 		 	iuo_variedad
uo_spro_ordenproceso iuo_ordenproceso
uo_productores			iuo_productores
uo_tratamientofrio	iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_lineapacking		iuo_lineapacking
uo_lotesfrutagranel	iuo_lotesfrutagranel

Boolean					ib_Modifica, ib_AutoCommit
DataWindowChild		idwc_especie, idwc_variedad, idwc_planta, &
							idwc_camara, idwc_envase, idwc_calidad

DataWindowChild		idwc_productor, idwc_periodofrio, idwc_tratamiento, &
							idwc_lineapacking

w_mant_deta_ordenproceso		iw_mantencion_1
w_mant_deta_programa_proc_cal	iw_consulta_1

Long						il_FilaDetProg
Integer 					ii_especie
end variables

forward prototypes
public subroutine habilitadetalle (string as_columna)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine chequeasaldo (string as_columna, string as_valor)
public function boolean existeprograma (string as_columna, string as_valor)
public function boolean existeprocesos (date adt_fechaproc)
public subroutine habilitaingreso (string as_columna)
public subroutine buscaprograma ()
end prototypes

public subroutine habilitadetalle (string as_columna);Boolean	lb_Estado = True

IF as_Columna <> "vari_codigo" AND &
	(dw_1.Object.vari_codigo[il_fila] = 0 OR IsNull(dw_1.Object.vari_codigo[il_fila])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "prod_codigo" AND &
	(dw_1.Object.prod_codigo[il_fila] = 0 OR IsNull(dw_1.Object.prod_codigo[il_fila])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "pefr_codigo" AND &
	(dw_1.Object.pefr_codigo[il_fila] = 0 OR IsNull(dw_1.Object.pefr_codigo[il_fila])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "frio_tipofr" AND &
	(dw_1.Object.frio_tipofr[il_fila] = "" OR IsNull(dw_1.Object.frio_tipofr[il_fila])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "line_codigo" AND &
	(dw_1.Object.line_codigo[il_fila] = 0 OR IsNull(dw_1.Object.line_codigo[il_fila])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "orpr_nrotur" AND &
	(dw_1.Object.orpr_nrotur[il_fila] = 0 OR IsNull(dw_1.Object.orpr_nrotur[il_fila])) THEN
	lb_Estado = False
END IF

//IF lb_estado THEN
//   dw_1.Object.b_detalle.Visible = True
//ELSE
//	dw_1.Object.b_detalle.Visible = False
//END IF
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.ppre_fecpro.Protect				=	0
	dw_2.Object.ppre_fecpro.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.Object.ppre_fecpro.Protect				=	1
	dw_2.Object.ppre_fecpro.BackGround.Color	=	RGB(192,192,192)
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_3.Update(True,False) =	1	THEN
		IF dw_1.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_3.ResetUpdate()
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
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_3.Update(True,False) =	1	THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_3.ResetUpdate()
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

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine chequeasaldo (string as_columna, string as_valor);Integer	li_vari_codigo, li_pefr_codigo
String	ls_frio_tipofr
Long		ll_prod_codigo 

li_Vari_codigo =  dw_1.Object.vari_codigo[il_Fila]
ll_Prod_codigo =	dw_1.Object.prod_codigo[il_Fila]
ls_frio_tipofr =	dw_1.Object.frio_tipofr[il_Fila]
li_pefr_codigo =	dw_1.Object.pefr_codigo[il_Fila]

CHOOSE CASE as_columna
	CASE "vari_codigo"
		li_Vari_Codigo = Integer(as_valor)
	CASE "prod_codigo"
		ll_Prod_Codigo = Long(as_valor)
	CASE "frio_tipofr"
		ls_frio_tipofr = as_valor
	CASE "pefr_codigo"
		li_pefr_codigo = Integer(as_valor)
END CHOOSE

iuo_lotesfrutagranel.SaldoExistencia(Integer(istr_Mant.Argumento[2]), &
li_Vari_codigo,ll_Prod_codigo, ls_frio_tipofr,li_pefr_codigo, SqlCa)

IF iuo_lotesfrutagranel.SaldoExistencia > 0 THEN
	dw_1.Object.Saldo[il_Fila]	=	iuo_lotesfrutagranel.SaldoExistencia
END IF

RETURN
end subroutine

public function boolean existeprograma (string as_columna, string as_valor);Integer	li_Planta, li_Especie
Long		ll_Numero
String	ls_Nombre
Boolean	lb_Retorno = True

li_Planta	=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_Planta	=	Integer(as_Valor)
		
	CASE "espe_codigo"
		li_Especie	=	Integer(as_Valor)
		
	CASE "ppre_numero"
		ll_Numero	=	Long(as_Valor)
		
END CHOOSE

IF Isnull(li_Planta) OR Isnull(li_Especie) OR IsNull(ll_Numero) THEN
	lb_Retorno	=	True
ELSE
	SELECT	ppre_nombre
		INTO	:ls_Nombre
		FROM	dba.spro_programaprocenca
		WHERE	plde_codigo	=	:li_Planta
		AND	espe_codigo	=	:li_Especie
		AND	ppre_numero	=	:ll_Numero;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Procesos")
		
		lb_Retorno	=	False
	ELSEIF SQLCA.SQLCode = 100 THEN
		MessageBox("Atención","Programa no ha sido registrado")
		lb_Retorno	=	False
	ELSE
		dw_2.Object.ppre_nombre[1]	=	ls_Nombre
//		dw_6.Retrieve(li_Planta,li_Especie,ll_Numero)
	
		dw_1.GetChild("vari_codigo",idwc_variedad)
		idwc_variedad.SetTransObject(Sqlca)
		IF idwc_variedad.Retrieve(li_Especie) = 0 THEN
			MessageBox("Atención","Falta Registrar Variedades")
			idwc_variedad.InsertRow(0)
		ELSE
			idwc_variedad.SetSort("vari_nombre A")
			idwc_variedad.Sort()
		END IF
		
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existeprocesos (date adt_fechaproc);Integer	li_Planta, li_Especie, li_Cantidad
Long		ll_Numero
Boolean	lb_Retorno = True

li_Planta	=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]

SELECT	Count(pprd_secuen)
	INTO	:li_Cantidad
	FROM	dba.spro_progordenproceso
	WHERE	plde_codigo	=	:li_Planta
	AND	espe_codigo	=	:li_Especie
	AND	ppre_numero	=	:ll_Numero
	AND	popr_fecpro	=	:adt_FechaProc;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Ordenes Procesos")
	
	lb_Retorno	=	False
ELSEIF SQLCA.SQLCode = 100 THEN
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date	ldt_Fecha

IF as_Columna <> "ppre_numero" AND &
	(dw_2.Object.ppre_numero[1] = 0 OR IsNull(dw_2.Object.ppre_numero[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "espe_codigo" AND &
	(dw_2.Object.espe_codigo[1] = 0 OR IsNull(dw_2.Object.espe_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "ppre_fecpro" AND &
	(dw_2.Object.ppre_fecpro[1] = ldt_Fecha OR IsNull(dw_2.Object.ppre_fecpro[1])) THEN
	lb_Estado = False
END IF

dw_1.Enabled			=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

public subroutine buscaprograma ();Str_Busqueda	lstr_busq
Date		ldt_Fecha

lstr_busq.argum[1] =	String(gstr_ParamPlanta.CodigoPlanta)

OpenWithParm(w_busc_spro_programaprocenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]

	dw_2.Object.espe_codigo[1]	=	Integer(lstr_Busq.Argum[2])
	dw_2.Object.ppre_numero[1]	=	Long(lstr_Busq.Argum[3])
	dw_2.Object.ppre_nombre[1]	=	lstr_Busq.Argum[5]

//	dw_6.Retrieve(Integer(lstr_Busq.Argum[1]),Integer(lstr_Busq.Argum[2]),Long(lstr_Busq.Argum[3]))

	dw_1.GetChild("vari_codigo",idwc_variedad)
	idwc_variedad.SetTransObject(Sqlca)
	IF idwc_variedad.Retrieve(Integer(lstr_Busq.Argum[2])) = 0 THEN
		MessageBox("Atención","Falta Registrar Variedades")
		idwc_variedad.InsertRow(0)
	ELSE
		idwc_variedad.SetSort("vari_nombre A")
		idwc_variedad.Sort()
	END IF
END IF

HabilitaIngreso("programa")
end subroutine

on w_maed_spro_orden_proceso_directo.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.st_1
end on

on w_maed_spro_orden_proceso_directo.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.st_1)
end on

event ue_nuevo();Long		ll_modif

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
					
			IF dw_1.RowCount() > 0 and ll_modif > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_3.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[3]  =  String(Date(Today()))

HabiliTaEncab(True)
end event

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Especie
istr_Mant.Argumento[3]	=	Número Programa
istr_Mant.Argumento[4]	=	Fecha Proceso
istr_Mant.Argumento[5]	=	Variedad
istr_Mant.Argumento[6]	=	Productor
istr_Mant.Argumento[7]	=	Periodo Frio
istr_Mant.Argumento[8]	=	Tratamiento Frio
istr_Mant.Argumento[9]	=	Numero de Orden
*/

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

iuo_especie					=	Create uo_especie
iuo_variedad				=	Create uo_variedades
iuo_ordenproceso			=	Create uo_spro_ordenproceso
iuo_productores			=	Create uo_productores
iuo_tratamientofrio		=	Create uo_tratamientofrio
iuo_periodofrio			=	Create uo_periodofrio
iuo_lineapacking			=	Create uo_lineapacking
iuo_lotesfrutagranel		=	Create uo_lotesfrutagranel

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	''
istr_Mant.Argumento[3]  =  String(Date(Today()))

dw_3.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.SetTransObject(sqlca)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)

IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	idwc_planta.InsertRow(0)
END IF

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_1.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
IF idwc_variedad.Retrieve(0) = 0 THEN
	MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
ELSE
	idwc_variedad.SetSort("vari_nombre A")
	idwc_variedad.Sort()
END IF

dw_1.Getchild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
IF idwc_productor.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
ELSE
	idwc_productor.SetSort("prod_nombre A")
	idwc_productor.Sort()
END IF

dw_1.Getchild("pefr_codigo", idwc_periodofrio)
idwc_periodofrio.SetTransObject(sqlca)
IF idwc_periodofrio.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Período Frío")
	idwc_periodofrio.InsertRow(0)
ELSE
	idwc_periodofrio.SetSort("pefr_nombre A")
	idwc_periodofrio.Sort()
END IF

dw_1.Getchild("frio_tipofr", idwc_tratamiento)
idwc_tratamiento.SetTransObject(sqlca)
IF idwc_tratamiento.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tratamiento Frío")
	idwc_tratamiento.InsertRow(0)
ELSE
	idwc_tratamiento.SetSort("frio_nombre A")
	idwc_tratamiento.Sort()
END IF

dw_1.Getchild("line_codigo", idwc_lineapacking)
idwc_lineapacking.SetTransObject(sqlca)
IF idwc_lineapacking.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Linea Packing")
	idwc_lineapacking.InsertRow(0)
ELSE
	idwc_lineapacking.SetSort("line_nombre A")
	idwc_lineapacking.Sort()
END IF

dw_3.Getchild("cama_codigo", idwc_camara)
idwc_camara.SetTransObject(sqlca)
IF idwc_camara.Retrieve(Integer(istr_Mant.Argumento[1])) = 0 THEN
	MessageBox("Atención","Falta Registrar Camara Frigorífico")
	idwc_camara.InsertRow(0)
ELSE
	idwc_camara.SetSort("cama_nombre A")
	idwc_camara.Sort()
END IF

dw_3.Getchild("cate_codigo", idwc_calidad)
idwc_calidad.SetTransObject(sqlca)
idwc_calidad.Retrieve()

dw_3.Getchild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(sqlca)
idwc_envase.Retrieve(0)

istr_Mant.dw				=	dw_3
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)
end event

event ue_recuperadatos();Long		ll_fila_d, ll_fila_e, respuesta
Date ldt_FechaProc

ldt_FechaProc	=	Date(Mid(istr_Mant.Argumento[4], 1, 10))

DO
	ll_fila_e	=	dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]))
										  
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						Information!, RetryCancel!)		
	ELSE

		DO
			IF dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
							     Integer(istr_Mant.Argumento[2]), &
								  ldt_FechaProc) = -1 THEN

				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				FOR il_Fila = 1 TO dw_1.RowCount()
					ChequeaSaldo("","")
				NEXT
				
				il_Fila	=	1
				dw_1.SetRow(il_Fila)
				
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True
				pb_eli_det.Enabled	=	True
				HabilitaEncab(False)
				dw_1.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_modifica_detalle();call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN

	istr_Mant.Agrega	= False
	istr_Mant.Borra	= False
	
	istr_Mant.Argumento[5] = String(dw_1.Object.vari_codigo[il_fila])
	istr_Mant.Argumento[6] = String(dw_1.Object.prod_codigo[il_fila])
	istr_Mant.Argumento[7] = String(dw_1.Object.pefr_codigo[il_fila])
	istr_Mant.Argumento[8] = dw_1.Object.frio_tipofr[il_fila]
	istr_Mant.Argumento[9] = String(dw_1.Object.orpr_numero[il_fila])
	dw_1.Object.espe_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])

	istr_Mant.dw	=	dw_1
	istr_Mant.dw2	=	dw_3

	OpenWithParm(iw_Mantencion_1, istr_Mant)

END IF
end event

event ue_borra_detalle();call super::ue_borra_detalle;Integer	li_Fila, li_CtaFila, li_BorraFila

IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	li_CtaFila = dw_3.RowCount()

	FOR li_Fila = 1 TO li_CtaFila
		li_BorraFila	=	dw_3.Find("orpr_numero = " + String(dw_1.Object.orpr_numero[il_Fila]), &
											 1, dw_3.RowCount())

		IF li_BorraFila > 0 THEN
			dw_3.DeleteRow(li_Fila)
		END IF
	NEXT

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
	END IF
END IF
end event

event ue_borrar();IF dw_1.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

ib_borrar = False
w_main.SetMicroHelp("Borrando Registro...")
		
IF wf_actualiza_db(True) THEN
	w_main.SetMicroHelp("Registro Borrado...")
	This.TriggerEvent("ue_nuevo")
	SetPointer(Arrow!)
ELSE
	w_main.SetMicroHelp("Registro no Borrado...")
END IF
end event

event ue_seleccion();str_Busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]
lstr_Busq.Argum[2]	=	'0'
lstr_Busq.Argum[3]	=	'4'

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	istr_mant.argumento[2] = lstr_busq.argum[4]
	istr_mant.argumento[4] = lstr_busq.argum[5]

	dw_2.Object.plde_codigo[1]	=	Integer(lstr_Busq.Argum[3])
	dw_2.Object.espe_codigo[1]	=	Integer(lstr_Busq.Argum[4])
	dw_2.Object.ppre_numero[1]	=	Long(lstr_Busq.Argum[11])
	dw_2.Object.ppre_fecpro[1]	=	Date(Mid(lstr_busq.argum[5],1,10))
	
	ExistePrograma("ppre_numero",lstr_Busq.Argum[11])
	
	TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF

IF ib_ok = False THEN RETURN
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Long	ll_Numero

dw_1.SetColumn("vari_codigo")

IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

il_fila = dw_1.InsertRow(0)

ll_Numero	=	iuo_OrdenProceso.MaximoNumero(Integer(istr_Mant.Argumento[1]), &
															4, Sqlca,gi_codexport)

IF dw_1.RowCount() = 1 THEN
	dw_1.Object.orpr_numero[il_Fila]	=	ll_Numero
ELSE
	dw_1.Object.orpr_numero[il_Fila]	=	dw_1.Object.orpr_numero[il_Fila - 1] + 1
END IF

dw_1.Object.orpr_estado[il_Fila] = 1

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
dw_1.SetColumn(1)

//IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
	pb_eli_det.Enabled		=	True
END IF
end event

event ue_antesguardar();call super::ue_antesguardar;Long			ll_Fila, ll_Numero
Integer		li_Planta, li_Especie, li_Secuencia
Date  		ldt_FechaProc

li_Planta		=	Integer(istr_Mant.Argumento[1])
li_Especie		=	Integer(istr_Mant.Argumento[2])
ll_Numero		=	Long(istr_Mant.Argumento[3])
ldt_FechaProc	=	Date(Mid(istr_Mant.Argumento[4], 1, 10))

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.plde_codigo[ll_Fila]	=	li_Planta
		dw_1.Object.orpr_tipord[ll_Fila]	=	4
		dw_1.Object.espe_codigo[ll_Fila]	=	li_Especie
		dw_1.Object.ppre_numero[ll_Fila]	=	ll_Numero
		dw_1.Object.orpr_fecpro[ll_Fila]	=	ldt_FechaProc
		dw_1.Object.orpr_estado[ll_fila] =  1
	END IF
NEXT

FOR ll_Fila = 1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_Fila]	=	li_Planta
		dw_3.Object.orpr_tipord[ll_Fila]	=	4
	END IF
NEXT
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
Date  	ldt_FechaProc

istr_info.titulo	= "INFORME PROCESOS DIARIOS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_programa_procesos_diarios"

vinf.dw_1.SetTransObject(sqlca)

ldt_FechaProc	=	Date(Mid(istr_Mant.Argumento[4], 1, 10))

fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3]),&
								  ldt_FechaProc)

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

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event resize;//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_orden_proceso_directo
integer x = 37
integer y = 1168
integer width = 2917
integer height = 700
integer taborder = 10
string title = "Detalle de Orden de Proceso"
string dataobject = "dw_mant_spro_ordenproceso_directo"
end type

event dw_1::doubleclicked;//
end event

event dw_1::itemchanged;String	ls_Columna, ls_Nula

ls_Columna = dwo.name

SetNull(ls_Nula)

CHOOSE CASE ls_Columna

	CASE "vari_codigo"
		IF NOT iuo_variedad.Existe(Integer(istr_Mant.Argumento[2]),Integer(Data),True,SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			ChequeaSaldo(ls_Columna,data)
		END IF

	CASE "prod_codigo"
		IF NOT iuo_productores.Existe(Long(Data),True,SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE
			ChequeaSaldo(ls_Columna,data)
		END IF

	CASE "frio_tipofr"
		IF NOT iuo_tratamientofrio.ofp_recupera_tratamientofrio(SqlCa,Data,True) THEN
			This.SetItem(il_Fila, ls_Columna, ls_Nula)
			RETURN 1
		ELSE
			ChequeaSaldo(ls_Columna,data)
		END IF

	CASE "pefr_codigo"
		IF NOT iuo_periodofrio.ofp_recupera_periodofrio(SqlCa,Integer(Data),True) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE
			ChequeaSaldo(ls_Columna,data)
		END IF

	CASE "line_codigo"
		IF NOT iuo_lineapacking.existe(Integer(istr_Mant.Argumento[1]), &
												 Integer(Data),True,SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF

END CHOOSE

Habilitadetalle(ls_Columna)
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event dw_1::getfocus;//
end event

event dw_1::itemerror;RETURN 1
end event

event dw_1::buttonclicked;call super::buttonclicked;Integer	li_Contador
String	ls_Mensaje, ls_Columna[]

CHOOSE CASE dwo.Name
	
	CASE "b_detalle"
		IF IsNull(This.Object.vari_codigo[il_fila]) OR This.Object.vari_codigo[il_fila] = 0 THEN
		  	li_Contador ++
			ls_Mensaje 					+=	"~nVariedad"
			ls_Columna[li_Contador]	=	"vari_codigo"
	   END IF
      
		IF IsNull(This.Object.prod_codigo[il_fila]) OR This.Object.prod_codigo[il_fila] = 0 THEN
		  	li_Contador ++
			ls_Mensaje 					+=	"~nProductor"
			ls_Columna[li_Contador]	=	"prod_codigo"
	   END IF

		IF IsNull(This.Object.frio_tipofr[il_fila]) OR This.Object.frio_tipofr[il_fila] = "" THEN
		  	li_Contador ++
			ls_Mensaje 					+=	"~nTratamiento Frío"
			ls_Columna[li_Contador]	=	"frio_tipofr"
	   END IF

		IF IsNull(This.Object.pefr_codigo[il_fila]) OR This.Object.pefr_codigo[il_fila] = 0 THEN
		  	li_Contador ++
			ls_Mensaje 					+=	"~nPeríodo Frío"
			ls_Columna[li_Contador]	=	"pefr_codigo"
	   END IF

		IF li_Contador > 0 THEN
			MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
			dw_1.SetColumn(ls_Columna[1])
			dw_1.SetFocus()
		ELSE
			IF iuo_lotesfrutagranel.SaldoExistencia > 0 OR dw_1.Object.orpr_canbul[il_Fila] > 0 THEN
				dw_1.Object.Saldo[il_Fila]	=	iuo_lotesfrutagranel.SaldoExistencia
				Parent.TriggerEvent("ue_modifica_detalle")
			ELSE
				MessageBox(	"Atención", "No Existe Saldo en Existencia", &
									Exclamation!, OK!)
			END IF
	   END IF	
END CHOOSE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_orden_proceso_directo
integer x = 325
integer width = 2565
integer height = 368
integer taborder = 90
string dataobject = "dw_sele_ordenproceso"
end type

event dw_2::itemchanged;Integer	li_Nula
String	ls_Columna, ls_Nula
Date ldt_FechaProc

ls_Columna = dwo.Name

SetNull(li_Nula)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "ppre_numero"
		IF NOT Existeprograma(ls_columna, data) THEN
			This.SetItem(row,ls_columna,li_Nula)
			RETURN 1
		ELSE
			istr_mant.argumento[3] = Data
		END IF	

	CASE "espe_codigo"	
		IF NOT iuo_Especie.Existe(Integer(Data),True,SqlCa) THEN  
			ii_especie	= integer(ls_Nula)
			istr_mant.argumento[2] = ""
			This.SetItem(row,"espe_codigo", li_Nula)
			This.SetFocus()
			RETURN 1
	   ELSE
			IF NOT Existeprograma(ls_columna, data) THEN
				This.SetItem(row,"espe_codigo", li_Nula)
				This.SetFocus()
				RETURN 1
			ELSE
				ii_especie = Integer(Data)
				istr_mant.argumento[2] = Data
			END IF	
		END IF	
	
	CASE "ppre_fecpro"
		istr_Mant.Argumento[4]	=	String(Date(Mid(Data,1,10)))
		ldt_FechaProc				=	Date(Mid(istr_Mant.Argumento[4], 1, 10))
		IF ExisteProcesos(ldt_FechaProc) THEN
			Parent.PostEvent("ue_recuperadatos")
		END IF
	
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name

	CASE "busca_programa"
		BuscaPrograma()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_orden_proceso_directo
integer x = 3282
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_orden_proceso_directo
integer x = 3282
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_orden_proceso_directo
integer x = 3282
integer taborder = 60
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_orden_proceso_directo
integer x = 3282
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_orden_proceso_directo
integer x = 3282
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_orden_proceso_directo
integer x = 3282
integer taborder = 20
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_orden_proceso_directo
integer x = 3282
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_orden_proceso_directo
integer x = 3282
integer taborder = 30
end type

type dw_3 from datawindow within w_maed_spro_orden_proceso_directo
boolean visible = false
integer x = 315
integer y = 1476
integer width = 2930
integer height = 412
integer taborder = 110
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_spro_ordenprocdeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_maed_spro_orden_proceso_directo
integer x = 512
integer y = 448
integer width = 2441
integer height = 720
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "Productores con Existencia"
string dataobject = "dw_mant_spro_productores_clasificados"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_maed_spro_orden_proceso_directo
integer x = 37
integer y = 448
integer width = 480
integer height = 720
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

