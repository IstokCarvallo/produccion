$PBExportHeader$w_maed_spro_orden_proceso_multi.srw
$PBExportComments$Ventana de mantención de Orden de Proceso de Fruta Granel.
forward
global type w_maed_spro_orden_proceso_multi from w_mant_encab_deta_csd
end type
type cb_detalle from commandbutton within w_maed_spro_orden_proceso_multi
end type
type cb_acepta from commandbutton within w_maed_spro_orden_proceso_multi
end type
type st_1 from statictext within w_maed_spro_orden_proceso_multi
end type
type dw_6 from uo_dw within w_maed_spro_orden_proceso_multi
end type
type dw_7 from datawindow within w_maed_spro_orden_proceso_multi
end type
type dw_3 from uo_dw within w_maed_spro_orden_proceso_multi
end type
type dw_8 from uo_dw within w_maed_spro_orden_proceso_multi
end type
type dw_4 from uo_dw within w_maed_spro_orden_proceso_multi
end type
type dw_5 from uo_dw within w_maed_spro_orden_proceso_multi
end type
end forward

global type w_maed_spro_orden_proceso_multi from w_mant_encab_deta_csd
integer width = 3849
integer height = 2048
string title = "Orden de Proceso"
string menuname = ""
windowstate windowstate = maximized!
cb_detalle cb_detalle
cb_acepta cb_acepta
st_1 st_1
dw_6 dw_6
dw_7 dw_7
dw_3 dw_3
dw_8 dw_8
dw_4 dw_4
dw_5 dw_5
end type
global w_maed_spro_orden_proceso_multi w_maed_spro_orden_proceso_multi

type variables
uo_especie 			 	iuo_especie
uo_variedades 		 	iuo_variedad
uo_grupoespecie    	iuo_grupo
uo_subgrupoespecie 	iuo_subgrupo
uo_spro_ordenproceso iuo_ordenproceso
uo_productores			iuo_productores
uo_tratamientofrio	iuo_tratamientofrio
uo_periodofrio			iuo_periodofrio
uo_lineapacking		iuo_lineapacking
uo_lotesfrutagranel	iuo_lotesfrutagranel

Long						il_FilaDetProg
Integer 					ii_especie, ii_grupo, ii_subgrupo, ii_TipoOrd
Date						ldt_fechaIni, ldt_fechater
Boolean					ib_Modifica, ib_AutoCommit, lb_Actualiza = False

DataWindowChild		idwc_especie, 		idwc_variedad, 	idwc_planta, 	idwc_periodofrio,	&
							idwc_categoria, 	idwc_grupo, 		idwc_subgrupo, idwc_embalaje, 	&
							idwc_etiqueta, 	idwc_recibidor, 	idwc_calibre, 	idwc_camara, 		&
							idwc_envase, 		idwc_calidad, 		idwc_cliente, 	idwc_productor, 	&
							idwc_tratamiento, idwc_lineapacking

w_mant_deta_ordenproceso_multiprod		iw_mantencion_1
w_mant_deta_programa_proc_cal				iw_consulta_1
end variables

forward prototypes
public subroutine habilitadetalle (string as_columna)
public subroutine muestradet_programa ()
public function boolean existeprocesos (date adt_fechaproc)
public subroutine chequeanuevaexistencia ()
public function boolean ordenposeedetalle (long al_orden)
protected function integer wf_modifica ()
public function boolean duplicado (string as_columna, string as_valor)
public subroutine revisaproceso ()
public subroutine habilitaencab (boolean habilita)
public function boolean existeprograma (string as_columna, string as_valor)
public subroutine habilitaingreso (string as_columna)
public subroutine buscaprograma ()
public function boolean buscamovto (long al_proceso)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean noexistecliente (integer cliente)
public subroutine chequeasaldodw_1 (string as_columna, string as_valor)
public subroutine chequeasaldo (string as_columna, string as_valor)
public subroutine recuperadetallemultiple (integer ai_fila)
public function boolean existecontratista (integer ai_valor)
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
end subroutine

public subroutine muestradet_programa ();str_Mant		lstr_Mant

IF dw_4.RowCount() > 0 THEN

	lstr_Mant.Agrega			= False
	lstr_Mant.Borra			= False
	lstr_Mant.Solo_Consulta	= True

	lstr_Mant.Argumento[1]	=	istr_Mant.Argumento[1]
	lstr_Mant.Argumento[2]	=	istr_Mant.Argumento[2]
	lstr_Mant.Argumento[3]	=	istr_Mant.Argumento[3]
	lstr_Mant.Argumento[4]	=	String(dw_4.Object.cate_codigo[il_FilaDetProg])
	lstr_Mant.Argumento[5]	=	String(dw_4.Object.etiq_codigo[il_FilaDetProg])
	lstr_Mant.Argumento[6]	=	dw_4.Object.emba_codigo[il_FilaDetProg]
	lstr_Mant.argumento[9] 	=  String(dw_4.Object.pprd_tipsel[il_FilaDetProg])
	lstr_Mant.argumento[10] =  String(dw_4.Object.pprd_cancaj[il_FilaDetProg])
	lstr_Mant.argumento[11] =  String(dw_4.Object.reci_codigo[il_FilaDetProg])
	lstr_Mant.argumento[12] =  String(dw_4.Object.pprd_secuen[il_FilaDetProg])
	lstr_Mant.argumento[13] =  istr_mant.argumento[13]
	
	lstr_Mant.dw	=	dw_4
	lstr_Mant.dw2	=	dw_6

	OpenWithParm(iw_consulta_1, lstr_Mant)
END IF
end subroutine

public function boolean existeprocesos (date adt_fechaproc);Integer	li_Planta, li_Especie, li_Cantidad, li_Cliente
Long		ll_Numero
Boolean	lb_Retorno = True

li_Planta	=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

SELECT	Count(sp.pprd_secuen)
	INTO	:li_Cantidad
	FROM	dbo.spro_progordenproceso as sp, 
			dbo.spro_programaprocenca as se
	WHERE	sp.plde_codigo	=	:li_Planta
	AND	sp.espe_codigo	=	:li_Especie
	AND	sp.ppre_numero	=	:ll_Numero
	AND   se.clie_codigo =  :li_Cliente	
	AND	sp.popr_fecpro	=	:adt_FechaProc
	AND   sp.plde_codigo	=	se.plde_codigo
	AND	sp.espe_codigo	=	se.espe_codigo
	AND	sp.ppre_numero	=  se.ppre_numero
	AND	se.clie_codigo	=	:li_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Ordenes Procesos")
	
	lb_Retorno	=	False
ELSEIF SQLCA.SQLCode = 100 THEN
	lb_Retorno	=	False
ELSE
	IF isnull(li_Cantidad) or li_cantidad=0 THEN lb_Retorno	=	False 	
END IF

RETURN lb_Retorno
end function

public subroutine chequeanuevaexistencia ();Integer	li_cont
Long     ll_fila
String	ls_mensaje

FOR ll_fila=1 To dw_1.RowCount()

	IF dw_1.Object.saldo[ll_fila] < 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~n"+string(dw_1.Object.orpr_numero[ll_fila])
	END IF

NEXT

IF li_cont > 0 THEN
	MessageBox("Atención", "Ordenes de Proceso :" + ls_mensaje + " Redujeron su Saldo en Existencia con respecto a su Creación.", Information!, Ok!)
END IF
end subroutine

public function boolean ordenposeedetalle (long al_orden);Long ll_Fila

ll_Fila	=	dw_3.Find("orpr_numero = "+String(al_orden), 1, dw_3.RowCount())
	
IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	RETURN True
ELSE
	RETURN False
END IF

end function

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF (dw_5.ModifiedCount() + dw_5.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila, ll_Productor
Integer	li_Variedad, li_Periodo, li_Cliente
String	ls_TipoFrio

li_Variedad		=	dw_7.Object.vari_codigo[il_Fila]
ll_Productor	=	dw_7.Object.prod_codigo[il_Fila]
li_Periodo		=	dw_7.Object.pefr_codigo[il_Fila]
ls_TipoFrio		=	dw_7.Object.frio_tipofr[il_Fila]
li_Cliente		=	Integer(istr_mant.argumento[13])


CHOOSE CASE as_Columna
	CASE "vari_codigo"
		li_Variedad		=	Integer(as_valor)
		
	CASE "prod_codigo"
		ll_Productor	=	Long(as_valor)
		
	CASE "pefr_codigo"
		li_Periodo		=	Integer(as_valor)
		
	CASE "frio_tipofr"
		ls_TipoFrio		=	as_valor
		
END CHOOSE

IF Isnull(li_Variedad) OR Isnull(ll_Productor) OR Isnull(li_Periodo) OR Isnull(ls_TipoFrio) THEN
	RETURN False
ELSE
	ll_Fila	=	dw_7.Find("vari_codigo = "+String(li_Variedad)+" and " + &
								 "clie_codigo = "+String(li_Cliente)+" and " + &
								 "prod_codigo = "+String(ll_Productor)+" and " + &
								 "pefr_codigo = "+String(li_Periodo)+" and " + &
								 "frio_tipofr = '"+ls_TipoFrio+"'", 1, dw_7.RowCount())
//prueba de ingreso LMP	
//	IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
//		MessageBox("Atención","Detalle de Proceso ya fue especificado")
//		RETURN True
//	ELSE
		RETURN False
//	END IF
END IF
end function

public subroutine revisaproceso ();Integer	li_Cantidad, li_fila, li_bloqueo, li_proceso, li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[13])

FOR li_fila = 1 TO dw_7.RowCount()
	li_Bloqueo = 1
	li_Proceso = Integer(dw_7.Object.orpr_numero[li_fila])
	
	SELECT Count(defg_docrel)
	INTO	 :li_Cantidad
	FROM	 dbo.spro_movtofrutagranenca
	WHERE	 defg_tipdoc = :ii_TipoOrd
	  AND  clie_codigo = :li_Cliente
	  AND  defg_docrel = :li_Proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Movtofrutagranenca")
		li_Bloqueo	=	0
	ELSEIF SQLCA.SQLCode = 100 THEN
		li_Bloqueo	=	0
	ELSEIF isnull(li_Cantidad) or li_cantidad=0 THEN 
		li_Bloqueo	=	0
	END IF
	
	dw_1.Object.bloqueo[li_fila] = li_Bloqueo

NEXT
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.busca_programa.Visible        	=  1
	
	dw_2.Object.espe_codigo.Protect				=	0
	dw_2.Object.ppre_numero.Protect				=	0
	dw_2.Object.ppre_fecpro.Protect				=	0
	dw_2.Object.clie_codigo.Protect				=	0
	
	dw_2.Object.espe_codigo.Color			=	0
	dw_2.Object.ppre_numero.Color			=	0
	dw_2.Object.ppre_fecpro.Color				=	0
	dw_2.Object.clie_codigo.Color				=	0
	
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_fecpro.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
ELSE
	dw_2.Object.busca_programa.Visible       	=  0
	
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.ppre_numero.Protect				=	1
	dw_2.Object.ppre_fecpro.Protect				=	1
	dw_2.Object.clie_codigo.Protect				=	1
	
	dw_2.Object.espe_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_numero.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_fecpro.Color		=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)
	
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.ppre_numero.BackGround.Color	=	553648127
	dw_2.Object.ppre_fecpro.BackGround.Color		=	553648127
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
END IF
end subroutine

public function boolean existeprograma (string as_columna, string as_valor);Integer	li_Planta, li_Especie, li_Cliente
Long		ll_Numero
String	ls_Nombre
Boolean	lb_Retorno = True
Date		ldt_fecha

li_Planta	=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

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
	SELECT	ppre_nombre, ppre_feccre
		INTO	:ls_Nombre, :ldt_fecha
		FROM	dbo.spro_programaprocenca
		WHERE	plde_codigo	=	:li_Planta
		AND	espe_codigo	=	:li_Especie
		AND	ppre_numero	=	:ll_Numero
		AND   clie_codigo =  :li_Cliente;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Procesos")
		
		lb_Retorno	=	False
	ELSEIF SQLCA.SQLCode = 100 THEN
		MessageBox("Atención","Programa no ha sido registrado")
		lb_Retorno	=	False
	ELSE
		dw_2.Object.ppre_nombre[1]	=	ls_Nombre
		dw_4.Retrieve(li_Planta,li_Especie,ll_Numero,li_Cliente)
		dw_6.Retrieve(li_Planta,li_Especie,ll_Numero,li_Cliente)

	
		dw_1.GetChild("vari_codigo",idwc_variedad)
		idwc_variedad.SetTransObject(Sqlca)
		IF idwc_variedad.Retrieve(li_Especie) = 0 THEN
			MessageBox("Atención","Falta Registrar Variedades")
			idwc_variedad.InsertRow(0)
		ELSE
			idwc_variedad.SetSort("vari_nombre A")
			idwc_variedad.Sort()
		END IF
		
		dw_7.GetChild("vari_codigo",idwc_variedad)
		idwc_variedad.SetTransObject(Sqlca)
		IF idwc_variedad.Retrieve(li_Especie) = 0 THEN
			MessageBox("Atención","Falta Registrar Variedades")
			idwc_variedad.InsertRow(0)
		ELSE
			idwc_variedad.SetSort("vari_nombre A")
			idwc_variedad.Sort()
		END IF
		
		PostEvent("ue_recuperadatos")
	END IF
END IF

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ldt_Fecha

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
Date				ldt_Fecha
Integer			li_Cliente

li_Cliente		=	dw_2.Object.clie_codigo[1]

lstr_busq.argum[1] =	String(dw_2.Object.plde_codigo[1])

IF ISnull(dw_2.Object.espe_codigo[1]) THEN
	lstr_busq.argum[2] = ""
ELSE
	lstr_busq.argum[2] = String(dw_2.Object.espe_codigo[1]) 
END IF

lstr_busq.argum[3] = String(li_Cliente) 
	
OpenWithParm(w_busc_spro_programaprocenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) >= 5 THEN
	
	IF lstr_busq.argum[2] <> "" THEN
		istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
		istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	
		dw_2.Object.espe_codigo[1]	=	Integer(lstr_Busq.Argum[2])
		dw_2.Object.ppre_numero[1]	=	Long(lstr_Busq.Argum[3])
		dw_2.Object.ppre_nombre[1]	=	lstr_Busq.Argum[5]
		dw_2.Object.ppre_fecpro[1] =	Date(lstr_Busq.Argum[4])
		
		dw_4.Retrieve(Integer(lstr_Busq.Argum[1]),Integer(lstr_Busq.Argum[2]),Long(lstr_Busq.Argum[3]),li_Cliente)
		dw_6.Retrieve(Integer(lstr_Busq.Argum[1]),Integer(lstr_Busq.Argum[2]),Long(lstr_Busq.Argum[3]),li_Cliente)
	
		dw_1.GetChild("vari_codigo",idwc_variedad)
		idwc_variedad.SetTransObject(Sqlca)
		IF idwc_variedad.Retrieve(Integer(lstr_Busq.Argum[2])) = 0 THEN
			MessageBox("Atención","Falta Registrar Variedades")
			idwc_variedad.InsertRow(0)
		ELSE
			idwc_variedad.SetSort("vari_nombre A")
			idwc_variedad.Sort()
		END IF
		istr_Mant.Argumento[4]	=	lstr_Busq.Argum[4]
		PostEvent("ue_recuperadatos")
	END IF
END IF
HabilitaIngreso("programa")
end subroutine

public function boolean buscamovto (long al_proceso);Integer  li_Cantidad, li_cliente
Boolean  lb_Retorno

li_cliente	=	Integer(istr_mant.argumento[13])

Select 	Count(*)
Into		:li_cantidad
From		dbo.spro_movtofrutagranenca
Where		defg_docrel = :al_Proceso
And      defg_tipdoc = 4
And      clie_codigo = :li_Cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	lb_Retorno	=	False

ELSEIF li_Cantidad = 0 THEN
	lb_Retorno  = False

ELSEIF li_Cantidad > 0 THEN
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean		lb_AutoCommit, lb_Retorno
DataStore	lds_detalleorden, lds_detallemulti
Long			ll_filas, ll_lote, ll_find
Integer		li_especie, li_planta

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

lds_detalleorden	=	Create DataStore
lds_detallemulti	=	Create DataStore

lds_detalleorden.DataObject	=	dw_3.DataObject
lds_detallemulti.DataObject	=	dw_8.DataObject

lds_detalleorden.SetTransObject(SQLCA)
lds_detallemulti.SetTransObject(SQLCA)

IF dw_3.DeletedCount()>0 THEN
	//Vacía Buffer Delete! de Detalle
	dw_3.RowsMove(1,dw_3.DeletedCount(),Delete!,lds_detalleorden,1,Primary!)
	
	//Busca los lotes eliminados del detalle dentro de detallemulti
	FOR ll_filas = 1 TO lds_detalleorden.RowCount()
		ll_lote		=	lds_detalleorden.GetItemNumber(ll_filas, "lote_codigo")
		li_especie	=	lds_detalleorden.GetItemNumber(ll_filas, "lote_espcod")
		li_planta	=	lds_detalleorden.GetItemNumber(ll_filas, "lote_pltcod")
		ll_find		=	dw_8.Find("lote_codigo = " + String(ll_lote) + " AND " + &
										 "lote_espcod = " + String(li_especie) + " AND " + &
										 "lote_pltcod = " + String(li_planta), 1, dw_8.RowCount())
	//Si el lote esta marcado como eliminado en la tabla de detalle, aca se elimina también
		IF ll_find > 0 THEN
			dw_8.RowsMove(ll_find, ll_find, Primary!, lds_detallemulti,1, Primary!)
		END IF
	NEXT
	
	//Inicializa los Flags a NotModified! (Estaban en NewModified! al mover las filas)
	lds_detalleorden.ResetUpdate()
	lds_detallemulti.ResetUpdate()
	
	//Pasa las filas al Buffer Delete! para forzar una eliminación previa del detalle	
	lds_detalleorden.RowsMove(1,lds_detalleorden.RowCount(),Primary!,lds_detalleorden,1,Delete!)
	lds_detallemulti.RowsMove(1,lds_detallemulti.RowCount(),Primary!,lds_detallemulti,1,Delete!)
	
END IF


IF Borrando THEN
	IF lds_detallemulti.Update(True, False) = 1 THEN
		IF dw_5.Update(True, False) = 1 THEN
			IF dw_3.Update(True,False) =	1	THEN
				IF lds_detalleorden.Update(True,False) = 1 THEN
					IF dw_7.Update(True, False) = 1 THEN
						Commit;
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							lb_Retorno	=	True
							dw_1.ResetUpdate()
							dw_3.ResetUpdate()
							dw_5.ResetUpdate()
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
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
ELSE
	IF dw_5.Update(True, False) = 1 THEN
		IF lds_detalleorden.Update(True,False) = 1 THEN
			IF dw_7.Update(True,False) =	1 THEN
				IF dw_3.Update(True, False) = 1 THEN
					IF lds_detallemulti.Update(True, False) = 1 THEN
						IF dw_8.Update(True, False) = 1 THEN
							Commit;
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
							ELSE
								lb_Retorno	=	True
								dw_1.ResetUpdate()
								dw_3.ResetUpdate()
								dw_5.ResetUpdate()
								dw_8.ResetUpdate()
								dw_7.ResetUpdate()
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

sqlca.AutoCommit	=	lb_AutoCommit

Destroy lds_detalleorden

RETURN lb_Retorno
end function

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public subroutine chequeasaldodw_1 (string as_columna, string as_valor);Integer	li_vari_codigo, li_pefr_codigo, li_Cliente
String	ls_frio_tipofr
Long     ll_bultos, ll_prod_codigo

//dw_1.Object.Saldo[il_Fila] = 0

li_Vari_codigo =  dw_1.Object.vari_codigo[il_Fila]
ll_Prod_codigo =	dw_1.Object.prod_codigo[il_Fila]
ls_frio_tipofr =	dw_1.Object.frio_tipofr[il_Fila]
li_pefr_codigo =	dw_1.Object.pefr_codigo[il_Fila]
ll_Bultos      =  dw_1.Object.orpr_canbul[il_fila]
li_Cliente		=	Integer(istr_mant.argumento[13])

IF isnull(ll_Bultos) THEN ll_Bultos = 0

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
	dw_1.Object.Saldo[il_Fila]	=	iuo_lotesfrutagranel.SaldoExistencia - ll_Bultos
END IF

RETURN
end subroutine

public subroutine chequeasaldo (string as_columna, string as_valor);Integer	li_vari_codigo, li_pefr_codigo, li_Cliente
String	ls_frio_tipofr
Long     ll_bultos, ll_prod_codigo

//dw_7.Object.Saldo[il_Fila] = 0

li_Vari_codigo =  dw_7.Object.vari_codigo[il_Fila]
ll_Prod_codigo =	dw_7.Object.prod_codigo[il_Fila]
ls_frio_tipofr =	dw_7.Object.frio_tipofr[il_Fila]
li_pefr_codigo =	dw_7.Object.pefr_codigo[il_Fila]
ll_Bultos      =  dw_7.Object.orpr_canbul[il_fila]
li_Cliente		=	Integer(istr_mant.argumento[13])

IF isnull(ll_Bultos) THEN ll_Bultos = 0

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
	dw_7.Object.Saldo[il_Fila]	=	iuo_lotesfrutagranel.SaldoExistencia - ll_Bultos
END IF

RETURN
end subroutine

public subroutine recuperadetallemultiple (integer ai_fila);Boolean 					lb_Modifica = TRUE
Long						ll_filas, ll_ubicado, ll_bultos
Integer					li_productor, li_numero, li_variedad, li_NroOrden, li_planta, li_cliente

uo_lotesfrutagranel	luo_lotesfrutagranel

IF IsNull(dw_7.Object.orpr_numero[il_fila]) OR dw_7.Object.orpr_numero[il_fila] < 1 THEN
	dw_3.SetFilter("orpr_numero = -1" )
ELSE
	dw_3.SetFilter("orpr_numero = " + String(dw_7.Object.orpr_numero[il_fila]))
END IF
dw_3.Filter()
dw_3.SetSort("prod_codigo asc")
dw_3.Sort()

dw_1.Reset()

IF dw_3.RowCount() > 0 THEN
	luo_lotesfrutagranel = Create uo_lotesfrutagranel

	FOR ll_filas = 1 TO dw_3.RowCount()
		li_productor	=	dw_3.Object.prod_codigo[ll_filas]
		li_numero		=	dw_3.Object.orpr_numero[ll_filas]
		ll_bultos		=	dw_3.Object.orpd_canbul[ll_filas]
		luo_lotesfrutagranel.existe(dw_3.Object.lote_pltcod[ll_filas], &
											 dw_3.Object.lote_espcod[ll_filas], &
											 dw_3.Object.lote_codigo[ll_filas], true, sqlca)

		li_variedad		=	luo_lotesfrutagranel.variedad
		
		IF dw_3.Getitemstatus(ll_filas,0,Primary!) = NewModified! THEN
			ll_ubicado = dw_8.InsertRow(0)
			dw_8.object.orpd_secuen[ll_ubicado]	=	ll_ubicado
			dw_8.Object.clie_codigo[ll_ubicado]	=	dw_2.Object.clie_codigo[1]
			dw_8.Object.orpr_tipord[ll_ubicado]	=	4
			dw_8.Object.orpr_numero[ll_ubicado]	=	li_numero
			dw_8.Object.orpr_tippro[ll_ubicado]	=	dw_7.Object.orpr_tippro[il_fila]
			dw_8.Object.vari_codigo[ll_ubicado]	=	li_variedad
			dw_8.Object.prod_codigo[ll_ubicado]	=	li_productor
			dw_8.Object.pefr_codigo[ll_ubicado]	=	dw_7.Object.pefr_codigo[il_fila]
			dw_8.Object.frio_tipofr[ll_ubicado]	=	dw_7.Object.frio_tipofr[il_fila]
			dw_8.Object.line_codigo[ll_ubicado]	=	dw_7.Object.line_codigo[il_fila]
			dw_8.Object.orpr_nrotur[ll_ubicado]	=	dw_7.Object.orpr_nrotur[il_fila]
			dw_8.Object.orpr_niveld[ll_ubicado]	=	dw_7.Object.orpr_niveld[il_fila]
			dw_8.Object.lote_totbul[ll_ubicado]	=	ll_bultos
			dw_8.Object.saldo[ll_ubicado]			=	dw_7.Object.saldo[il_fila]
			dw_8.Object.plde_codigo[ll_ubicado]	=	dw_3.Object.lote_pltcod[ll_filas]
			dw_8.Object.lote_pltcod[ll_ubicado]	=	dw_3.Object.lote_pltcod[ll_filas]
			dw_8.Object.lote_espcod[ll_ubicado]	=	dw_3.Object.lote_espcod[ll_filas]
			dw_8.Object.lote_codigo[ll_ubicado]	=	dw_3.Object.lote_codigo[ll_filas]
			dw_8.Object.ppre_numero[ll_ubicado]	=	dw_7.Object.ppre_numero[il_Fila]	
			dw_8.Object.ppre_feccre[ll_ubicado]	=	dw_7.Object.orpr_fecpro[il_Fila]
			dw_8.Setitemstatus(ll_ubicado,0,Primary!,NewModified!)
		END IF

		ll_ubicado		=	dw_1.Find("orpr_numero = " + String(li_numero) + " AND " + &
											 "prod_codigo = " + String(li_productor) + " AND " + &
											 "vari_codigo = " + String(li_variedad), &
											 1, dw_1.RowCount())

		IF ll_ubicado = 0 THEN 
			ll_ubicado = dw_1.InsertRow(0)
			dw_1.Object.lote_totbul[ll_ubicado] = 	0
			dw_1.object.orpd_secuen[ll_ubicado]	=	ll_ubicado
		END IF

		dw_1.Object.orpr_numero[ll_ubicado]	=	li_numero
		dw_1.Object.orpr_tippro[ll_ubicado]	=	dw_7.Object.orpr_tippro[il_fila]
		dw_1.Object.vari_codigo[ll_ubicado]	=	li_variedad
		dw_1.Object.prod_codigo[ll_ubicado]	=	li_productor
		dw_1.Object.pefr_codigo[ll_ubicado]	=	dw_7.Object.pefr_codigo[il_fila]
		dw_1.Object.frio_tipofr[ll_ubicado]	=	dw_7.Object.frio_tipofr[il_fila]
		dw_1.Object.line_codigo[ll_ubicado]	=	dw_7.Object.line_codigo[il_fila]
		dw_1.Object.orpr_nrotur[ll_ubicado]	=	dw_7.Object.orpr_nrotur[il_fila]
		dw_1.Object.orpr_niveld[ll_ubicado]	=	dw_7.Object.orpr_niveld[il_fila]
		dw_1.Object.lote_totbul[ll_ubicado]	=	dw_1.Object.lote_totbul[ll_ubicado] + ll_bultos
		dw_1.Object.saldo[ll_ubicado]			=	dw_7.Object.saldo[il_fila]

	NEXT

	Destroy luo_lotesfrutagranel
END IF

dw_3.SetFilter("")
dw_3.Filter()
dw_3.SetSort("lote_pltcod asc, lote_espcod asc, lote_codigo asc, "+&
				 "enva_tipoen asc, enva_codigo asc, cate_codigo asc, "+&
				 "cama_codigo asc")
dw_3.Sort()

//IF dw_7.GetitemStatus(il_fila,0,Primary!) = Notmodified! THEN
//	dw_7.Object.orpr_canbul[il_fila] = Long(istr_Mant.argumento[10])
//	dw_7.Object.saldo[il_fila]       = Long(istr_Mant.argumento[11])
//END IF	
dw_1.Accepttext()
dw_1.Enabled 	=	False
pb_ins_det.setfocus()
dw_7.SetColumn("orpr_niveld")
dw_7.SetFocus()
end subroutine

public function boolean existecontratista (integer ai_valor);Integer	li_existe, li_codigo
Boolean 	lb_retorno

lb_retorno	=	TRUE
li_codigo		=	ai_valor

SELECT Count(*) 
INTO :li_existe FROM dbo.contratista
WHERE cont_codigo = :li_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Contratistas")
	lb_Retorno	=	False
ELSEIF SQLCA.SQLCode = 100 THEN
	lb_Retorno	=	False
ELSE
	IF isnull(li_existe) or li_existe=0 THEN lb_Retorno	=	False 	
END IF

Return lb_retorno
end function

on w_maed_spro_orden_proceso_multi.create
int iCurrent
call super::create
this.cb_detalle=create cb_detalle
this.cb_acepta=create cb_acepta
this.st_1=create st_1
this.dw_6=create dw_6
this.dw_7=create dw_7
this.dw_3=create dw_3
this.dw_8=create dw_8
this.dw_4=create dw_4
this.dw_5=create dw_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_detalle
this.Control[iCurrent+2]=this.cb_acepta
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.dw_6
this.Control[iCurrent+5]=this.dw_7
this.Control[iCurrent+6]=this.dw_3
this.Control[iCurrent+7]=this.dw_8
this.Control[iCurrent+8]=this.dw_4
this.Control[iCurrent+9]=this.dw_5
end on

on w_maed_spro_orden_proceso_multi.destroy
call super::destroy
destroy(this.cb_detalle)
destroy(this.cb_acepta)
destroy(this.st_1)
destroy(this.dw_6)
destroy(this.dw_7)
destroy(this.dw_3)
destroy(this.dw_8)
destroy(this.dw_4)
destroy(this.dw_5)
end on

event ue_nuevo;Long		ll_modif

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
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()

dw_3.GetChild("pprc_calini", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
idwc_calibre.Retrieve(-1,-1,-1)

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True
lb_Actualiza 				=  False

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.clie_codigo[1]	=	gi_CodExport

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	String(gi_Codespecie)
istr_Mant.Argumento[3]  =  String(Today(),'dd/mm/yyyy')

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
istr_Mant.Argumento[13]	=	Cliente
*/

x				= 0
y				= 0
This.Height	= 2520
im_menu		= m_principal
ii_TipoOrd 	=	Integer(Message.StringParm)

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

iuo_especie					=	Create uo_especie
iuo_variedad				=	Create uo_variedades
iuo_grupo					=	Create uo_grupoespecie
iuo_subgrupo				=	Create uo_subgrupoespecie
iuo_ordenproceso			=	Create uo_spro_ordenproceso
iuo_productores			=	Create uo_productores
iuo_tratamientofrio		=	Create uo_tratamientofrio
iuo_periodofrio				=	Create uo_periodofrio
iuo_lineapacking			=	Create uo_lineapacking
iuo_lotesfrutagranel		=	Create uo_lotesfrutagranel

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	String(gi_codespecie)
istr_Mant.Argumento[3]  =  String(Today(),'dd/mm/yyyy')
istr_Mant.Argumento[13] =  String(gi_codexport)

dw_8.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
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
IF idwc_variedad.Retrieve(gstr_paramplanta.codigoespecie) = 0 THEN
	MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
ELSE
	idwc_variedad.SetSort("vari_nombre A")
	idwc_variedad.Sort()
END IF

dw_7.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
IF idwc_variedad.Retrieve(gstr_paramplanta.codigoespecie) = 0 THEN
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

dw_7.Getchild("prod_codigo", idwc_productor)
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

dw_7.Getchild("pefr_codigo", idwc_periodofrio)
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

dw_7.Getchild("frio_tipofr", idwc_tratamiento)
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

dw_7.Getchild("line_codigo", idwc_lineapacking)
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

//Cliente
dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
idwc_cliente.InsertRow(0)

dw_3.Getchild("cate_codigo", idwc_calidad)
idwc_calidad.SetTransObject(sqlca)
idwc_calidad.Retrieve()

dw_3.Getchild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(sqlca)
idwc_envase.Retrieve(0)

dw_6.GetChild("pprc_calini", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
idwc_calibre.Retrieve(-1,-1,-1)

istr_Mant.dw				=	dw_3
istr_Mant.solo_consulta =	False

dw_4.SetTabOrder("reci_codigo",0)
dw_4.SetTabOrder("todosrec",0)
dw_4.SetTabOrder("cate_codigo",0)
dw_4.SetTabOrder("emba_codigo",0)
dw_4.SetTabOrder("etiq_codigo",0)
dw_4.SetTabOrder("todasetq",0)
dw_4.SetTabOrder("pprd_plu",0)
dw_4.SetTabOrder("pprd_adhesi",0)

Paramtemporada(gstr_paramtempo)
ldt_fechaIni = gstr_paramtempo.fechainicio
ldt_fechater = gstr_paramtempo.fechatermino

IF ii_TipoOrd 	= 8 THEN
	THIS.Title 	= "Orden de PreProceso"
	dw_1.Title 	= "Detalle de Orden de Pre Proceso"
ELSE
	THIS.Title 	= "Orden de Proceso"
	dw_1.Title 	= "Detalle de Orden de Proceso"
END IF

pb_nuevo.PostEvent(Clicked!)
end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta
Date		ldt_FechaProc

ldt_FechaProc	=	Date(Mid(istr_Mant.Argumento[4], 1, 10))
istr_Mant.Argumento[13] = string(dw_2.object.clie_codigo[1])

DO
	ll_fila_e	=	dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]), &
										  ldt_FechaProc,Integer(istr_Mant.Argumento[13]))
										  
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						Information!, RetryCancel!)		
	ELSE
		IF ll_fila_e = 0 THEN
			cb_acepta.Visible	=	True
			dw_4.Visible		=	True
			dw_5.Visible		=	False
		ELSE
			dw_5.Visible		=	True
			cb_acepta.Visible	=	False
		END IF
		
		DO
			IF dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
							     Integer(istr_Mant.Argumento[2]), &
								  ldt_FechaProc,Integer(istr_Mant.Argumento[13])) = -1 OR &
				dw_7.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  ldt_FechaProc, Long(istr_Mant.Argumento[3]),Integer(istr_Mant.Argumento[13])) = -1 OR &
				dw_8.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  ldt_FechaProc, Long(istr_Mant.Argumento[3]),Integer(istr_Mant.Argumento[13])) = -1 THEN

				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				//verifica se bloquea modificacion
				RevisaProceso()
				FOR il_Fila = 1 TO dw_1.RowCount()
					ChequeaSaldodw_1("","")
					dw_1.Setitemstatus(il_fila,0,Primary!,Notmodified!)
				NEXT
				dw_2.Setitemstatus(1,0,Primary!,Notmodified!)
				ChequeaNuevaExistencia()
								
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True
				pb_eli_det.Enabled	=	True
				HabilitaEncab(False)
				
				il_Fila	=	1
				dw_1.SetRow(il_Fila)
				dw_1.SetColumn("vari_codigo")
				dw_1.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;Boolean 	lb_Modifica = TRUE
Long		ll_filas, ll_ubicado, ll_bultos, ll_minimo
Integer	li_productor, li_numero, li_variedad
uo_lotesfrutagranel	luo_lotesfrutagranel

IF dw_7.RowCount() > 0 THEN

	istr_Mant.Agrega	= False
	istr_Mant.Borra	= False
	istr_Mant.Argumento[5] = String(dw_7.Object.vari_codigo[il_fila])
	istr_Mant.Argumento[6] = String(dw_7.Object.prod_codigo[il_fila])
	istr_Mant.Argumento[7] = String(dw_7.Object.pefr_codigo[il_fila])
	istr_Mant.Argumento[8] = dw_7.Object.frio_tipofr[il_fila]
	istr_Mant.Argumento[9] = String(dw_7.Object.orpr_numero[il_fila])
	istr_Mant.argumento[10]= String(dw_7.Object.orpr_canbul[il_fila])	
	istr_Mant.argumento[11]= String(dw_7.Object.saldo[il_fila])	
	
	IF dw_1.Getitemstatus(il_fila,0,Primary!)=Notmodified! THEN lb_modifica=FALSE
	
	dw_7.Object.espe_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
	dw_7.Object.clie_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[13])
   dw_7.Object.orpr_fecpro[il_Fila]	=	dw_2.Object.ppre_fecpro[1]
	dw_7.Object.ppre_numero[il_Fila]	=	dw_2.Object.ppre_numero[1]
	
	IF NOT lb_modifica THEN dw_7.Setitemstatus(il_fila,0,Primary!,Notmodified!)
	
	istr_Mant.dw	=	dw_7
	istr_Mant.dw2	=	dw_3

	OpenWithParm(iw_Mantencion_1, istr_Mant)
	
	dw_3.SetFilter("orpr_numero = " + istr_Mant.Argumento[9])
	dw_3.Filter()
	dw_3.SetSort("prod_codigo asc")
	dw_3.Sort()
	
	dw_1.Reset()
	
	RecuperaDetalleMultiple(il_Fila)

END IF
end event

event ue_borra_detalle;Integer	li_Fila, li_CtaFila, li_BorraFila, li_estado

IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	li_estado = dw_7.object.orpr_estado[il_fila]
	IF isnull(li_estado) OR li_estado = 0 THEN 
		li_estado=1
	END IF
	
	IF li_estado=1 AND Not buscaMovto(dw_7.Object.orpr_numero[il_Fila]) THEN
		li_CtaFila = dw_3.RowCount()
		
		FOR li_Fila = 1 TO li_CtaFila
			li_BorraFila	=	dw_3.Find("orpr_numero = " + String(dw_7.Object.orpr_numero[il_Fila]), &
												 1, dw_3.RowCount())
	
			IF li_BorraFila > 0 THEN
				dw_3.DeleteRow(li_BorraFila)
			END IF
		NEXT
	
		IF dw_7.DeleteRow(0) = 1 THEN
			ib_borrar = False
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE 
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
	 	IF dw_7.RowCount() = 0 THEN
			pb_eliminar.Enabled = False
		ELSE
			il_fila = dw_7.GetRow()
		END IF
	ELSE
		MessageBox(This.Title,"La orden No se puede eliminar.")
	END IF	
END IF
end event

event ue_borrar;IF dw_1.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_8.RowCount() > 0 THEN dw_8.RowsMove(1,dw_8.RowCount(),Primary!,dw_8,1,Delete!)

IF dw_7.RowCount() > 0 THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)

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

event ue_seleccion;call super::ue_seleccion;str_Busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]
lstr_Busq.Argum[2]	=	'0'
lstr_Busq.Argum[3]	=	String(ii_TipoOrd)
lstr_Busq.Argum[4]	=	istr_Mant.Argumento[13]

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	istr_mant.argumento[2] = lstr_busq.argum[4]
	istr_mant.argumento[3] = lstr_Busq.Argum[11]
	istr_mant.argumento[4] = lstr_busq.argum[5]

	dw_2.Object.plde_codigo[1]	=	Integer(lstr_Busq.Argum[3])
	dw_2.Object.espe_codigo[1]	=	Integer(lstr_Busq.Argum[4])
	dw_2.Object.ppre_numero[1]	=	Long(lstr_Busq.Argum[11])
	dw_2.Object.ppre_fecpro[1]	=	Date(Mid(lstr_busq.argum[5],1,10))
	
	IF ExistePrograma("ppre_numero",lstr_Busq.Argum[11]) THEN
	
		TriggerEvent("ue_recuperadatos")
	END IF
	
	dw_1.SetFocus()
	
ELSE
	pb_buscar.SetFocus()
END IF

//IF ib_ok = False THEN RETURN
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Long	ll_Numero, ll_null

SetNull(ll_null)
dw_7.SetColumn("vari_codigo")

IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF


il_fila = dw_7.InsertRow(0)

ll_Numero	=	iuo_OrdenProceso.MaximoNumero(Integer(istr_Mant.Argumento[1]), &
															ii_TipoOrd, Sqlca, integer(istr_mant.argumento[13]))
IF il_fila = 1 THEn lb_Actualiza = False

IF ll_Numero > 0 THEN
   IF Not lb_Actualiza THEN
		dw_7.Object.orpr_numero[il_Fila]	=	ll_Numero
		lb_Actualiza = True
	ELSE
		dw_7.Object.orpr_numero[il_Fila]	=	dw_7.Object.orpr_numero[il_Fila - 1] + 1	
		dw_7.Object.orpr_tippro[il_fila] = 	dw_7.Object.orpr_tippro[il_fila - 1]
		dw_7.Object.vari_codigo[il_fila] = 	dw_7.Object.vari_codigo[il_fila - 1]
		dw_7.Object.prod_codigo[il_fila] = 	dw_7.Object.prod_codigo[il_fila - 1]
		dw_7.Object.frio_tipofr[il_fila] = 	dw_7.Object.frio_tipofr[il_fila - 1]
		dw_7.Object.pefr_codigo[il_fila] = 	dw_7.Object.pefr_codigo[il_fila - 1]
		dw_7.Object.line_codigo[il_fila] = 	dw_7.Object.line_codigo[il_fila - 1]
		dw_7.Object.orpr_nrotur[il_fila] = 	dw_7.Object.orpr_nrotur[il_fila - 1]
		dw_7.AcceptText()
	END IF
ELSE
	MessageBox("Error de Ingreso","No se pudo detectar la última orden de proceso. ~rCierre la ventana y vuelva a intentarlo.")
END IF

dw_7.Object.orpr_estado[il_Fila] 	= 	1
dw_7.Object.orpr_tipord[il_Fila] 	= 	4


dw_7.ScrollToRow(il_fila)
dw_7.SetRow(il_fila)
dw_7.SetFocus()
dw_7.SetColumn(1)

IF dw_7.RowCount() > 0 THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
	pb_eli_det.Enabled	=	True
END IF
end event

event ue_antesguardar;Long			ll_Fila, ll_Numero, ll_NroProc
Integer		li_Planta, li_Especie, li_Secuencia, li_Contador
Date			ldt_FechaProc
String		ls_Mensaje, ls_Columna[]

li_Planta				=	Integer(istr_Mant.Argumento[1])
li_Especie				=	Integer(istr_Mant.Argumento[2])
ll_Numero				=	Long(istr_Mant.Argumento[3])

ldt_FechaProc			=	Date(Mid(istr_Mant.Argumento[4], 1, 10))

Message.DoubleParm 	= 	0

FOR ll_Fila = 1 TO dw_7.RowCount()
	IF IsNull(dw_7.Object.vari_codigo[ll_fila]) OR dw_7.Object.vari_codigo[ll_fila] = 0 THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nVariedad"
		ls_Columna[li_Contador]	=	"vari_codigo"
	END IF
	
	IF IsNull(dw_7.Object.prod_codigo[ll_fila]) OR dw_7.Object.prod_codigo[ll_fila] = 0 THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nProductor"
		ls_Columna[li_Contador]	=	"prod_codigo"
	END IF
	
	IF IsNull(dw_7.Object.frio_tipofr[ll_fila]) OR dw_7.Object.frio_tipofr[ll_fila] = "" THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nTratamiento Frío"
		ls_Columna[li_Contador]	=	"frio_tipofr"
	END IF
	
	IF IsNull(dw_7.Object.pefr_codigo[ll_fila]) OR dw_7.Object.pefr_codigo[ll_fila] = 0 THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nPeríodo Frío"
		ls_Columna[li_Contador]	=	"pefr_codigo"
	END IF
	
	IF IsNull(dw_7.Object.line_codigo[ll_fila]) OR dw_7.Object.line_codigo[ll_fila] = 0 THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nLínea Packing"
		ls_Columna[li_Contador]	=	"line_codigo"
	END IF
	
	IF IsNull(dw_7.Object.orpr_nrotur[ll_fila]) OR dw_7.Object.orpr_nrotur[ll_fila] = 0 THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nTurno"
		ls_Columna[li_Contador]	=	"orpr_nrotur"
	END IF
	
	IF li_Contador > 0 THEN
		MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
		Message.DoubleParm = -1
		dw_7.SetColumn(ls_Columna[1])
		dw_7.SetRow(ll_fila)
		dw_7.SetFocus()
		RETURN
	END IF
NEXT


FOR ll_Fila = 1 TO dw_7.RowCount()
	IF dw_7.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_NroProc	=	dw_7.Object.orpr_numero[ll_Fila]
		
		IF IsNull(dw_7.Object.orpr_canbul[ll_Fila]) OR &
			dw_7.Object.orpr_canbul[ll_Fila] = 0 THEN
			MessageBox("Atención","Falta especificar los bultos para la Orden de Proceso "+&
						 String(ll_NroProc,'00000000'))
			dw_7.SetRow(ll_Fila)
			dw_7.ScrollToRow(ll_Fila)
			Message.DoubleParm	=	-1 
			RETURN
		ELSEIF IsNull(dw_7.Object.line_codigo[ll_Fila]) OR &
			dw_7.Object.line_codigo[ll_Fila] = 0 THEN
			MessageBox("Atención","Falta especificar Línea para la Orden de Proceso "+&
						 String(ll_NroProc,'00000000'))
			dw_7.SetRow(ll_Fila)
			dw_7.ScrollToRow(ll_Fila)
			Message.DoubleParm	=	-1 
			RETURN
		END IF
		
		dw_7.Object.plde_codigo[ll_Fila]	=	li_Planta
		dw_7.Object.orpr_tipord[ll_Fila]	=	ii_TipoOrd
		dw_7.Object.espe_codigo[ll_Fila]	=	li_Especie
		dw_7.Object.ppre_numero[ll_Fila]	=	ll_Numero
		dw_7.Object.orpr_fecpro[ll_Fila]	=	ldt_FechaProc
		dw_7.Object.orpr_estado[ll_fila] =  1
		dw_7.Object.clie_codigo[ll_fila] =  Integer(istr_mant.argumento[13])
	END IF
NEXT

FOR ll_Fila = 1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_Fila]	=	li_Planta
		dw_3.Object.orpr_tipord[ll_Fila]	=	ii_TipoOrd
		dw_3.Object.clie_codigo[ll_fila] =  Integer(istr_mant.argumento[13])
	END IF
NEXT

//1-6 - 2
FOR ll_fila = 1 to dw_7.RowCount()
	dw_1.Object.Clie_codigo[ll_fila] = Integer(istr_mant.argumento[13])
NEXT

FOR ll_fila = 1 to dw_3.RowCount()
	dw_3.Object.Clie_codigo[ll_fila] = Integer(istr_mant.argumento[13])
NEXT

FOR ll_fila = 1 to dw_4.RowCount()
	dw_4.Object.Clie_codigo[ll_fila] = Integer(istr_mant.argumento[13])
NEXT

FOR ll_fila = 1 to dw_5.RowCount()
	dw_5.Object.clie_codigo[ll_fila] = Integer(istr_mant.argumento[13])
	dw_5.Object.popr_fecpro[ll_Fila]	= ldt_FechaProc
NEXT

FOR ll_fila = 1 to dw_6.RowCount()
	dw_6.Object.clie_codigo[ll_fila] = Integer(istr_mant.argumento[13])
NEXT
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Date		ldt_FechaProc

istr_info.titulo	= "INFORME PROCESOS DIARIOS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

DataWindowChild	ldwc_especies

vinf.dw_1.DataObject = "dw_info_programa_procesos_diarios"

vinf.dw_1.GetChild("espe_codigo",ldwc_especies)
ldwc_especies.SetTransObject(SQLCA)
ldwc_especies.Retrieve(gi_codexport)

vinf.dw_1.SetTransObject(sqlca)

ldt_FechaProc	=	Date(Mid(istr_Mant.Argumento[4], 1, 10))

fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3]),&
								  ldt_FechaProc,Integer(istr_Mant.Argumento[13]),ii_TipoOrd)

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

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
	dw_7.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 80 + dw_2.Height + st_1.Height + dw_7.Height
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41

dw_4.Width				= dw_1.Width - st_1.Width - 50
dw_5.Width				= dw_1.Width - st_1.Width - 50
dw_1.Width				= dw_1.Width - 50
dw_7.Width				= dw_1.Width - st_1.Width - 50

dw_7.x					= dw_1.x
dw_7.y					= 64 + dw_2.Height + st_1.Height






end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_orden_proceso_multi
integer x = 37
integer y = 1380
integer width = 3223
integer height = 496
integer taborder = 10
boolean enabled = false
string title = "Detalle de Orden de Proceso"
string dataobject = "dw_mant_spro_ordenproceso_prog_multi_det"
end type

event dw_1::doubleclicked;//
end event

event dw_1::itemchanged;Integer	li_Variedad,li_periodo,li_line, li_turno, li_nivel, li_bultos
String	ls_Columna, ls_Nula, ls_frio
Long     ll_Saldo, ll_Canti, ll_Total, ll_Productor

ls_Columna = dwo.name

SetNull(ls_Nula)

CHOOSE CASE ls_Columna

	CASE "vari_codigo"
		li_variedad = dw_1.Object.vari_codigo[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row, "vari_codigo", li_variedad)
			RETURN 1
		ELSE
//			IF NOT iuo_variedad.Existe(Integer(istr_Mant.Argumento[2]),Integer(Data),True,SqlCa,Integer(istr_mant.argumento[13])) OR &
//				Duplicado(ls_Columna,data) THEN
			IF NOT iuo_variedad.Existe(Integer(istr_Mant.Argumento[2]),Integer(Data),True,SqlCa) OR &
				Duplicado(ls_Columna,data) THEN
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				This.SetFocus()
				RETURN 1
			ELSE
				ChequeaSaldoDw_1(ls_Columna,data)
			END IF
		END IF

	CASE "prod_codigo"
		ll_Productor = dw_1.Object.prod_codigo[row] 
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row, "prod_codigo", ll_productor)
			RETURN 1
		ELSE
			IF NOT iuo_productores.Existe(Long(Data),True,SqlCa) OR &
				Duplicado(ls_Columna,data) THEN
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				RETURN 1
			ELSE
				ChequeaSaldoDw_1(ls_Columna,data)
			END IF
		END IF
		
	CASE "frio_tipofr"
		ls_frio = dw_1.Object.frio_tipofr[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"frio_tipofr",ls_frio)
			RETURN 1
		ELSE
			IF NOT iuo_tratamientofrio.ofp_recupera_tratamientofrio(SqlCa,Data,True) OR &
				Duplicado(ls_Columna,data) THEN
				This.SetItem(Row, ls_Columna, ls_Nula)
				RETURN 1
			ELSE
				ChequeaSaldoDw_1(ls_Columna,data)
			END IF
		END IF
		
	CASE "pefr_codigo"
		li_periodo = dw_1.Object.pefr_codigo[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"pefr_codigo",li_periodo)
			RETURN 1
		ELSE
			IF NOT iuo_periodofrio.ofp_recupera_periodofrio(SqlCa,Integer(Data),True) OR &
				Duplicado(ls_Columna,data) THEN
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				RETURN 1
			ELSE
				ChequeaSaldoDw_1(ls_Columna,data)
			END IF
		END IF
		
	CASE "line_codigo"
		li_line = dw_1.Object.line_codigo[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"line_codigo",li_line)
			RETURN 1
		ELSE
			IF NOT iuo_lineapacking.existe(Integer(istr_Mant.Argumento[1]), &
													 Integer(Data),True,SqlCa) THEN
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				RETURN 1
			END IF
		END IF
		
	CASE "orpr_nrotur"
		li_turno = dw_1.Object.orpr_nrotur[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"orpr_nrotur",li_turno)
			RETURN 1
		END IF
		
	CASE "orpr_niveld"
		li_nivel = dw_1.Object.orpr_niveld[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"orpr_niveld",li_nivel)
			RETURN 1
		END IF		
		
	CASE "orpr_canbul"
		li_bultos = dw_1.Object.orpr_canbul[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"orpr_canbul",li_bultos)
			RETURN 1
		ELSE
			IF isnull(dw_1.Object.orpr_canbul[Row]) THEN
				ll_Canti = 0
			ELSE	
				ll_Canti = dw_1.Object.orpr_canbul[Row]
			END IF 
			
			IF ordenposeedetalle(dw_1.Object.orpr_numero[Row]) THEN
				MessageBox("Error","La Orden Posee Lotes Seleccionados en Detalle.")
				dw_1.Object.orpr_canbul[Row] = ll_Canti  
				Return 1
			END IF
			
			IF Long(Data)<=0 THEN
				MessageBox("Error","Ingrese Valores superiores a Cero.")
				dw_1.Object.orpr_canbul[Row] = ll_Canti  
				Return 1
			END IF	
			
			ll_Saldo = dw_1.Object.Saldo[Row]
			
			ll_total = ll_Canti + ll_Saldo
			
			IF Ceiling(Long(data)) > ll_total THEN
				MessageBox("Advertencia","El Valor Ingresado supera la cantidad ("+String(ll_total,"#,##0")+") en Existencia para los Datos Seleccionados.")
			  dw_1.Object.Saldo[Row] 	= (ll_total - Round(dec(data),0))
			ELSE
				dw_1.Object.Saldo[Row] 	= (ll_total - Round(dec(data),0))
			END IF 
		END IF
		
END CHOOSE


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
Long     ll_bultos

CHOOSE CASE dwo.Name
	
	CASE "b_detalle"
		This.AcceptText()
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			RETURN 1
		ELSE
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
	
			IF IsNull(This.Object.line_codigo[il_fila]) OR This.Object.line_codigo[il_fila] = 0 THEN
				li_Contador ++
				ls_Mensaje 					+=	"~nLínea Packing"
				ls_Columna[li_Contador]	=	"line_codigo"
			END IF
	
			IF IsNull(This.Object.orpr_nrotur[il_fila]) OR This.Object.orpr_nrotur[il_fila] = 0 THEN
				li_Contador ++
				ls_Mensaje 					+=	"~nTurno"
				ls_Columna[li_Contador]	=	"orpr_nrotur"
			END IF
			
			IF li_Contador > 0 THEN
				MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
				dw_1.SetColumn(ls_Columna[1])
				dw_1.SetFocus()
			ELSE
				ll_bultos = dw_1.Object.orpr_canbul[il_Fila]
				IF isnull(ll_bultos) THEN ll_Bultos = 0
				IF iuo_lotesfrutagranel.SaldoExistencia > 0 OR ll_bultos > 0 THEN
					dw_1.Object.Saldo[il_Fila]	=	iuo_lotesfrutagranel.SaldoExistencia - ll_bultos
					Parent.TriggerEvent("ue_modifica_detalle")
				ELSE
					IF MessageBox("Atención", "Puede que No Exista Saldo en Existencia.Desea Continuar", Question!, YesNo!) = 1 THEN
						dw_1.Object.Saldo[il_Fila]	=	iuo_lotesfrutagranel.SaldoExistencia - ll_bultos
						Parent.TriggerEvent("ue_modifica_detalle")					
				  END IF
				END IF 
			END IF	
		END IF
END CHOOSE
end event

event dw_1::ue_seteafila;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_orden_proceso_multi
integer x = 87
integer y = 40
integer width = 3127
integer height = 340
integer taborder = 90
string dataobject = "dw_sel_prograproceso_enca"
end type

event dw_2::itemchanged;Integer	li_Nula
String	ls_Columna, ls_Nula
Date		ldt_FechaProc, ldt_FechaSistema, ldt_fecha

ls_Columna = dwo.Name

SetNull(li_Nula)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		END IF
		istr_mant.Argumento[13] = data

		dw_2.GetChild("espe_codigo", idwc_especie)
		idwc_especie.SetTransObject(sqlca)
		idwc_especie.Retrieve()
		idwc_especie.InsertRow(0)
		
		dw_1.GetChild("vari_codigo",idwc_variedad)
		idwc_variedad.SetTransObject(Sqlca)
		idwc_variedad.Retrieve(Integer(istr_mant.Argumento[2]))
		idwc_variedad.InsertRow(0)

		dw_1.Getchild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(sqlca)
		idwc_productor.Retrieve()	
		idwc_productor.InsertRow(0)
		
		dw_5.GetChild("emba_codigo", idwc_embalaje)
		idwc_embalaje.SetTransObject(sqlca)
		idwc_embalaje.Retrieve()
		idwc_embalaje.InsertRow(0)
	
		dw_5.Getchild("reci_codigo", idwc_recibidor)
		idwc_recibidor.SetTransObject(sqlca)
		idwc_recibidor.Retrieve()
		idwc_recibidor.InsertRow(0)
		
		dw_5.Getchild("etiq_codigo", idwc_etiqueta)
		idwc_etiqueta.SetTransObject(sqlca)
		idwc_etiqueta.Retrieve()
		idwc_etiqueta.InsertRow(0)		
		
		dw_4.GetChild("emba_codigo", idwc_embalaje)
		idwc_embalaje.SetTransObject(sqlca)
		idwc_embalaje.Retrieve()
		idwc_embalaje.InsertRow(0)
	
		dw_4.Getchild("reci_codigo", idwc_recibidor)
		idwc_recibidor.SetTransObject(sqlca)
		idwc_recibidor.Retrieve()
		idwc_recibidor.InsertRow(0)
		
		dw_4.Getchild("etiq_codigo", idwc_etiqueta)
		idwc_etiqueta.SetTransObject(sqlca)
		idwc_etiqueta.Retrieve()
		idwc_etiqueta.InsertRow(0)		
	
	CASE "ppre_numero"
		IF NOT Existeprograma(ls_columna, data) THEN
			This.SetItem(row,ls_columna,li_Nula)
			RETURN 1
		ELSE
			istr_mant.argumento[3] = Data
		END IF	

	CASE "espe_codigo"	
		istr_mant.argumento[2] = ""
		IF NOT iuo_Especie.Existe(Integer(Data),True,SqlCa) THEN  
			ii_especie	= integer(ls_Nula)
			istr_mant.argumento[2] = ""
			This.SetItem(row,"espe_codigo", li_Nula)
			This.SetFocus()
			RETURN 1
	   ELSE
			IF NOT Existeprograma(ls_columna, data) THEN
				This.SetItem(row,"espe_codigo", li_Nula)
				istr_mant.argumento[2] = ""
				ii_especie	= integer(ls_Nula)
				This.SetFocus()
				RETURN 1
			ELSE
				ii_especie = Integer(Data)
				istr_mant.argumento[2] = Data
			END IF	
		END IF	
	
	CASE "ppre_fecpro"
		ldt_FechaProc		=	Date(Mid(Data, 1, 10))
		SetNull(ldt_fecha)
		IF ldt_FechaProc > ldt_fechater THEN
			MessageBox("Atención","Fecha de Movimiento no puede ser superior a Fecha de término de temporada.")
			This.SetItem(row,"ppre_fecpro", ldt_fecha)
			RETURN 1
		ELSE	
			IF ldt_FechaProc < ldt_fechaini THEN
				MessageBox("Atención","Fecha de Movimiento no puede ser menor a Fecha de inicio de temporada.")
				This.SetItem(row,"ppre_fecpro", ldt_fecha)
				RETURN 1
			ELSE
				istr_Mant.Argumento[4]	=	String(Date(Mid(Data,1,10)))
				IF ExisteProcesos(ldt_FechaProc) THEN
					Parent.PostEvent("ue_recuperadatos")
				END IF	
			END IF	
		END IF
	
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name

	CASE "busca_programa"
		BuscaPrograma()

END CHOOSE
end event

event dw_2::clicked;//
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_orden_proceso_multi
integer x = 3342
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_orden_proceso_multi
integer x = 3342
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_orden_proceso_multi
integer x = 3342
integer taborder = 60
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_orden_proceso_multi
integer x = 3342
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_orden_proceso_multi
integer x = 3342
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_orden_proceso_multi
integer x = 3342
integer taborder = 20
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_orden_proceso_multi
integer x = 3342
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_orden_proceso_multi
integer x = 3342
integer taborder = 30
end type

type cb_detalle from commandbutton within w_maed_spro_orden_proceso_multi
integer x = 64
integer y = 464
integer width = 233
integer height = 88
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Detalle"
end type

event clicked;Long	ll_Fila, ll_FilaDet
Date		ld_fecha

IF NOT isnull(dw_2.Object.espe_codigo[1]) AND NOT isnull(dw_2.Object.ppre_numero[1])ANd &
   dw_2.Object.ppre_fecpro[1] <> ld_fecha  THEN
	dw_4.SelectRow(0,False)
	
	FOR ll_Fila = 1 TO dw_5.RowCount()
		ll_FilaDet	=	dw_4.Find("pprd_secuen = "+String(dw_5.Object.pprd_secuen[ll_Fila]),1,dw_4.RowCount())
		
		IF ll_FilaDet > 0 THEN
			dw_4.SelectRow(ll_FilaDet,True)
		END IF
	NEXT
	
	dw_4.Visible		=	True
	dw_5.Visible		=	False
	cb_acepta.Visible	=	True
END IF	
end event

type cb_acepta from commandbutton within w_maed_spro_orden_proceso_multi
integer x = 64
integer y = 632
integer width = 233
integer height = 88
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Acepta"
end type

event clicked;Integer	li_CtaFila, li_Fila, li_FilaNueva
Long		ll_Fila1, ll_Fila2
Date		ldt_FechaProceso

IF NOT IsNull(dw_2.Object.ppre_fecpro[1]) THEN
	ldt_FechaProceso	=	Date(Mid(istr_Mant.Argumento[4], 1, 10))

	ll_Fila2 = dw_4.GetSelectedRow(0)

	IF ll_Fila2 = 0 AND dw_5.RowCount() = 0 THEN
		MessageBox("Error de Consistencia","Debe seleccionar el Detalle del Programa")
		RETURN
	END IF
	
	SetPointer(HourGlass!)
	
	li_CtaFila =	dw_4.RowCount()
	
	dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
	
	FOR li_Fila = 1 TO li_CtaFila
		IF dw_4.IsSelected(li_Fila) THEN
			
			li_FilaNueva	=	dw_5.InsertRow(0)
	
			dw_5.Object.plde_codigo[li_FilaNueva] = dw_4.Object.plde_codigo[li_Fila]
			dw_5.Object.espe_codigo[li_FilaNueva] = dw_4.Object.espe_codigo[li_Fila]
			dw_5.Object.ppre_numero[li_FilaNueva] = dw_4.Object.ppre_numero[li_Fila]
			dw_5.Object.pprd_secuen[li_FilaNueva] = dw_4.Object.pprd_secuen[li_Fila]
			dw_5.Object.popr_fecpro[li_FilaNueva] = ldt_FechaProceso
			dw_5.Object.reci_codigo[li_FilaNueva] = dw_4.Object.reci_codigo[li_Fila]
			dw_5.Object.cate_codigo[li_FilaNueva] = dw_4.Object.cate_codigo[li_Fila]
			dw_5.Object.emba_codigo[li_FilaNueva] = dw_4.Object.emba_codigo[li_Fila]
			dw_5.Object.enva_tipoen[li_FilaNueva] = dw_4.Object.enva_tipoen[li_Fila]
			dw_5.Object.enva_codigo[li_FilaNueva] = dw_4.Object.enva_codigo[li_Fila]
			dw_5.Object.enva_nombre[li_FilaNueva] = dw_4.Object.enva_nombre[li_Fila]
			dw_5.Object.etiq_codigo[li_FilaNueva] = dw_4.Object.etiq_codigo[li_Fila]
		   dw_5.Object.clie_codigo[li_FilaNueva] = Integer(istr_mant.argumento[13])
			
			IF isnull(dw_4.Object.pprd_plu[li_Fila]) THEN
				dw_5.Object.pprd_plu[li_FilaNueva] = 0
			ELSE	
			   dw_5.Object.pprd_plu[li_FilaNueva] = dw_4.Object.pprd_plu[li_Fila]
			END IF	
			
			dw_5.Object.pprd_adhesi[li_FilaNueva] = dw_4.Object.pprd_adhesi[li_Fila]
		
		END IF
	NEXT
	
	This.Visible	=	False
	dw_4.Visible	=	False
	dw_5.Visible	=	True
	
	dw_5.SetFocus()
ELSE
	MessageBox("Atención","Debe ingresar Previamente Fecha de Proceso")
	dw_2.SetFocus()
	dw_2.SetColumn("ppre_fecpro")
END IF
end event

type st_1 from statictext within w_maed_spro_orden_proceso_multi
integer x = 37
integer y = 396
integer width = 283
integer height = 476
integer textsize = -9
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

type dw_6 from uo_dw within w_maed_spro_orden_proceso_multi
boolean visible = false
integer x = 873
integer y = 324
integer width = 210
integer height = 216
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_prograproceso_cal"
boolean vscrollbar = false
borderstyle borderstyle = stylebox!
end type

event add_row;//
end event

event clicked;//
end event

event del_all_rows;//
end event

event del_row;//
end event

event sqlpreview;//
end event

type dw_7 from datawindow within w_maed_spro_orden_proceso_multi
integer x = 41
integer y = 880
integer width = 3218
integer height = 496
integer taborder = 80
boolean bringtotop = true
boolean titlebar = true
string title = "Selección del Detalle"
string dataobject = "dw_mant_spro_ordenproceso_programa_multi"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Variedad,li_periodo,li_line, li_turno, li_nivel, li_bultos
String	ls_Columna, ls_Nula, ls_frio
Long     ll_Saldo, ll_Canti, ll_Total, ll_Productor

ls_Columna = dwo.name

SetNull(ls_Nula)

CHOOSE CASE ls_Columna

	CASE "vari_codigo"
		li_variedad = dw_7.Object.vari_codigo[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row, "vari_codigo", li_variedad)
			RETURN 1
		ELSE
			IF NOT iuo_variedad.Existe(Integer(istr_Mant.Argumento[2]),Integer(Data),True,SqlCa) OR &
				Duplicado(ls_Columna,data) THEN
				This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
				This.SetFocus()
				RETURN 1
			ELSE
				ChequeaSaldo(ls_Columna,data)
			END IF
		END IF

	CASE "prod_codigo"
		ll_Productor = dw_7.Object.prod_codigo[row] 
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row, "prod_codigo", ll_productor)
			RETURN 1
		ELSEIF Long(Data) <> -1 THEN			
			IF NOT iuo_productores.Existe(Long(Data),True,SqlCa) OR &
				Duplicado(ls_Columna,data) THEN
				This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
				RETURN 1
			ELSE
				ChequeaSaldo(ls_Columna,data)
			END IF
//		ELSE
//			ChequeaSaldo(ls_Columna,data)
		END IF
		
	CASE "frio_tipofr"
		ls_frio = dw_7.Object.frio_tipofr[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"frio_tipofr",ls_frio)
			RETURN 1
		ELSE
			IF NOT iuo_tratamientofrio.ofp_recupera_tratamientofrio(SqlCa,Data,True) OR &
				Duplicado(ls_Columna,data) THEN
				This.SetItem(il_Fila, ls_Columna, ls_Nula)
				RETURN 1
			ELSE
				ChequeaSaldo(ls_Columna,data)
			END IF
		END IF
		
	CASE "pefr_codigo"
		li_periodo = dw_7.Object.pefr_codigo[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"pefr_codigo",li_periodo)
			RETURN 1
		ELSE
			IF NOT iuo_periodofrio.ofp_recupera_periodofrio(SqlCa,Integer(Data),True) OR &
				Duplicado(ls_Columna,data) THEN
				This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
				RETURN 1
			ELSE
				ChequeaSaldo(ls_Columna,data)
			END IF
		END IF
		
	CASE "line_codigo"
		li_line = dw_7.Object.line_codigo[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"line_codigo",li_line)
			RETURN 1
		ELSE
			IF NOT iuo_lineapacking.existe(Integer(istr_Mant.Argumento[1]), &
													 Integer(Data),True,SqlCa) THEN
				This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
				RETURN 1
			END IF
		END IF
		
	CASE "orpr_nrotur"
		li_turno = dw_7.Object.orpr_nrotur[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"orpr_nrotur",li_turno)
			RETURN 1
		END IF
		
	CASE "orpr_niveld"
		li_nivel = dw_7.Object.orpr_niveld[row]
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			This.SetItem(row,"orpr_niveld",li_nivel)
			RETURN 1
		END IF		
		
//	CASE "orpr_canbul"
//		li_bultos = dw_7.Object.orpr_canbul[row]
//		IF This.Object.bloqueo[row] = 1 THEN
//			Messagebox("Error","Registro no es posible modificarlo")
//			This.SetItem(row,"orpr_canbul",li_bultos)
//			RETURN 1
//		ELSE
//			IF isnull(dw_7.Object.orpr_canbul[il_Fila]) THEN
//				ll_Canti = 0
//			ELSE	
//				ll_Canti = dw_7.Object.orpr_canbul[il_Fila]
//			END IF 
//			
//			IF ordenposeedetalle(dw_7.Object.orpr_numero[il_fila]) THEN
//				MessageBox("Error","La Orden Posee Lotes Seleccionados en Detalle.")
//				dw_7.Object.orpr_canbul[il_Fila] = ll_Canti  
//				Return 1
//			END IF
//			
//			IF Long(Data)<=0 THEN
//				MessageBox("Error","Ingrese Valores superiores a Cero.")
//				dw_7.Object.orpr_canbul[il_Fila] = ll_Canti  
//				Return 1
//			END IF	
//			
//			ll_Saldo = dw_7.Object.Saldo[il_Fila]
//			
//			ll_total = ll_Canti + ll_Saldo
//			
//			IF Ceiling(Long(data)) > ll_total THEN
//				MessageBox("Advertencia","El Valor Ingresado supera la cantidad ("+String(ll_total,"#,##0")+") en Existencia para los Datos Seleccionados.")
//			  dw_7.Object.Saldo[il_Fila] = (ll_total - Round(dec(data),0))
//			ELSE
//				dw_7.Object.Saldo[il_Fila] = (ll_total - Round(dec(data),0))
//			END IF 
//		END IF
	CASE "cont_codigo"
		IF NOT ExisteContratista(Integer(data)) THEN
			This.SetItem(row, ls_columna, Integer(ls_nula))
			Return 1
		END IF
END CHOOSE


end event

event buttonclicked;Integer	li_Contador
String	ls_Mensaje, ls_Columna[]
Long     ll_bultos

CHOOSE CASE dwo.Name
	CASE "b_detalle"
		This.AcceptText()
		IF This.Object.bloqueo[row] = 1 THEN
			Messagebox("Error","Registro no es posible modificarlo")
			RETURN 1
		ELSE
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
	
			IF IsNull(This.Object.line_codigo[il_fila]) OR This.Object.line_codigo[il_fila] = 0 THEN
				li_Contador ++
				ls_Mensaje 					+=	"~nLínea Packing"
				ls_Columna[li_Contador]	=	"line_codigo"
			END IF
	
			IF IsNull(This.Object.orpr_nrotur[il_fila]) OR This.Object.orpr_nrotur[il_fila] = 0 THEN
				li_Contador ++
				ls_Mensaje 					+=	"~nTurno"
				ls_Columna[li_Contador]	=	"orpr_nrotur"
			END IF
			
			IF li_Contador > 0 THEN
				MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
				dw_7.SetColumn(ls_Columna[1])
				dw_7.SetFocus()
			ELSE
				ll_bultos = dw_7.Object.orpr_canbul[il_Fila]
				IF isnull(ll_bultos) THEN ll_Bultos = 0
				IF iuo_lotesfrutagranel.SaldoExistencia > 0 OR ll_bultos > 0 THEN
					dw_7.Object.Saldo[il_Fila]	=	iuo_lotesfrutagranel.SaldoExistencia - ll_bultos
					Parent.TriggerEvent("ue_modifica_detalle")
				ELSE
					IF MessageBox("Atención", "Puede que No Exista Saldo en Existencia.Desea Continuar", Question!, YesNo!) = 1 THEN
						dw_7.Object.Saldo[il_Fila]	=	iuo_lotesfrutagranel.SaldoExistencia - ll_bultos
						Parent.TriggerEvent("ue_modifica_detalle")					
				  END IF
				END IF 
			END IF	
		END IF
END CHOOSE
end event

event rowfocuschanged;IF CurrentRow > 0 THEN
	il_fila 	=	CurrentRow
	recuperadetallemultiple(CurrentRow)
END IF
end event

type dw_3 from uo_dw within w_maed_spro_orden_proceso_multi
boolean visible = false
integer x = 667
integer y = 328
integer width = 210
integer height = 216
integer taborder = 11
boolean bringtotop = true
string title = "dw_3"
string dataobject = "dw_mues_spro_ordenprocdeta"
boolean vscrollbar = false
borderstyle borderstyle = stylebox!
end type

event add_row;//
end event

event clicked;//
end event

event sqlpreview;//
end event

event del_all_rows;//
end event

event del_row;//
end event

type dw_8 from uo_dw within w_maed_spro_orden_proceso_multi
boolean visible = false
integer x = 1138
integer y = 324
integer width = 210
integer height = 216
integer taborder = 11
boolean bringtotop = true
string title = "dw_8"
string dataobject = "dw_mant_spro_ordenproceso_prog_multi_det"
boolean vscrollbar = false
borderstyle borderstyle = stylebox!
end type

type dw_4 from uo_dw within w_maed_spro_orden_proceso_multi
integer x = 329
integer y = 396
integer width = 2880
integer height = 472
integer taborder = 11
boolean titlebar = true
string title = "Detalle del Programa"
string dataobject = "dw_mant_prograproceso_deta"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event buttonclicking;call super::buttonclicking;CHOOSE CASE dwo.Name
	CASE "b_detalle"
		IF Row > 0 THEN
			il_FilaDetProg = Row
			MuestraDet_Programa()
		END IF
	
END CHOOSE
end event

event clicked;IF row > 0 THEN
	IF IsSelected(row) THEN
		SelectRow(row,False)
	ELSE
		SelectRow(row,True)
	END IF
END IF
end event

type dw_5 from uo_dw within w_maed_spro_orden_proceso_multi
integer x = 329
integer y = 396
integer width = 2880
integer height = 472
integer taborder = 11
boolean titlebar = true
string title = "Programa a Proceso"
string dataobject = "dw_mues_spro_progordenproceso"
boolean hscrollbar = true
boolean livescroll = true
end type

event add_row;//
end event

event clicked;//
end event

event del_all_rows;//
end event

event del_row;//
end event

event sqlpreview;//
end event

