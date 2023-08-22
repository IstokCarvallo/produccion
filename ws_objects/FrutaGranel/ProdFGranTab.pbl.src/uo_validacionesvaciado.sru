$PBExportHeader$uo_validacionesvaciado.sru
forward
global type uo_validacionesvaciado from nonvisualobject
end type
end forward

global type uo_validacionesvaciado from nonvisualobject
end type
global uo_validacionesvaciado uo_validacionesvaciado

type variables
Integer	lote, especie, TipoEnva, Envase
Long 		bins, fgmb_tibapa, planta, plantalote, lotetarja
String	calidad

str_mant	istr_mant
end variables

forward prototypes
public function boolean cargabins (integer ai_cliente, integer ai_planta, long al_bins, boolean ab_mensaje, transaction at_transaction)
public function boolean binsduplicados (integer ai_cliente, integer ai_planta, long al_nrotarja, boolean ab_mensaje, transaction at_transaction)
public function boolean validalote (integer ai_planta, integer ai_cliente, integer ai_tipoord, integer ai_proceso, long al_lote, boolean ab_mensaje, transaction at_transaccion)
public function boolean validalote (integer ai_planta, integer ai_cliente, integer ai_tipoord, integer ai_proceso, long al_lote, integer ai_tipenv, integer ai_codenv, boolean ab_mensaje, transaction at_transaccion)
public subroutine controlmultiplanta (integer ai_cliente, long al_planta, long al_tarja)
public function boolean validalote (integer ai_planta, integer ai_cliente, integer ai_tipoord, integer ai_proceso, long al_lote, integer ai_tipenv, integer ai_codenv, integer ai_pltcod, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean cargabins (integer ai_cliente, integer ai_planta, long al_bins, boolean ab_mensaje, transaction at_transaction);SELECT	enva_tipoen, enva_codigo, cale_calida
	INTO	:TipoEnva, :Envase, :Calidad
	FROM	dbo.spro_bins
	WHERE clie_codigo	=	:ai_Cliente
	  AND	plde_codigo	=	:ai_Planta
	  AND	bins_numero	=	:al_Bins
		USING at_transaction;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_ordenprocdeta" )
	Return False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_mensaje THEN
		Messagebox("Error", "El Bins " + String(al_Bins, '00000000') + " " +&
								  "No existe en la tabla de bins. Ingrese otra tarja", Information!)
	END IF
	Return False
END IF

Return True
end function

public function boolean binsduplicados (integer ai_cliente, integer ai_planta, long al_nrotarja, boolean ab_mensaje, transaction at_transaction);Long	ll_cantidad

SELECT max(lote_pltcod), Count(fgmb_nrotar)
INTO	:plantalote, :ll_cantidad
	FROM	dbo.spro_movtobins
	WHERE 	clie_codigo		=	:ai_Cliente
	  	AND	plde_codigo		=	:ai_Planta
		AND	fgmb_nrotar		=	:al_nrotarja
		AND 	fgmb_estado		> 	0
		USING at_transaction;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtobins" )
	Return False
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	IF ab_mensaje THEN
		messagebox("Atención","No Existe Tarja  " + String(al_nrotarja, '00000000') + " " + &
									 "en Tabla de Movimiento de Bins o no esta Vigente")
	END IF
	
	Return False		
ELSEIF ll_cantidad > 1 THEN
	
	ControlMultiplanta(ai_cliente, ai_planta, al_nrotarja)
	
	IF IsNull(plantalote) OR plantalote < 1 THEN
		IF ab_mensaje THEN
			messagebox("Atención","La Tarja  " + String(al_nrotarja, '00000000') + " " + &
										 "se encuentra Duplicada en Tabla de Movimiento de Bins " + &
										 "y no se selecciono la planta de origen, para hacer distinción.")			
		END IF
		Return False
	END IF
END IF

IF NOT IsNull(plantalote) AND plantalote > 0 THEN 
	SELECT	bins_numero, lote_codigo, lote_espcod, fgmb_tibapa
		INTO	:Bins, :Lote, :Especie, :fgmb_tibapa
		FROM	dbo.spro_movtobins
		WHERE 	clie_codigo		=	:ai_Cliente
			AND	plde_codigo		=	:ai_Planta
			AND	fgmb_nrotar		=	:al_nrotarja
			AND 	lote_pltcod		=	:plantalote
			AND 	fgmb_estado		> 	0
			AND 	IsNull(:especie, 0) in (lote_espcod, 0)
			AND 	IsNull(:lotetarja, 0) in (lote_codigo, 0);
			
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtobins" )
		Return False
		
	ELSEIF sqlca.SQLCode = 100 THEN
		IF ab_mensaje THEN
			Messagebox("Atención","No Existe Tarja  " + String(al_nrotarja, '00000000') + " " + &
										 "en Tabla de Movimiento de Bins o no esta Vigente")			
		END IF
		
		Return False	
	END IF
END IF

Return True
end function

public function boolean validalote (integer ai_planta, integer ai_cliente, integer ai_tipoord, integer ai_proceso, long al_lote, boolean ab_mensaje, transaction at_transaccion);Long	ll_cantidad

SELECT count(*)
INTO	:ll_cantidad
	FROM	dbo.spro_ordenprocdeta_cajasprod
	WHERE 	plde_codigo		=	:ai_planta
		AND	clie_codigo		=	:ai_cliente
		AND	orpr_tipord		=	:ai_TipoOrd	
		AND	orpr_numero		=	:ai_proceso
		AND 	capr_estado		=	1
		USING at_transaccion;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_ordenprocdeta_cajasprod" )
	Return False			
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	IF ab_mensaje THEN
		messagebox("Atención","No existen tarjas aprobadas para este proceso. ~r~nFavor de comunicar a Encargado de Packing")			
	END IF
	
	Return False		
END IF

SELECT lote_codigo
INTO	:ll_cantidad
	FROM	dbo.spro_ordenprocdeta
	WHERE 	plde_codigo		=	:ai_planta
		AND	clie_codigo		=	:ai_cliente
		AND	orpr_tipord		=	:ai_TipoOrd	
		AND	orpr_numero		=	:ai_proceso
		AND 	lote_codigo		=	:al_lote
		USING at_transaccion;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_OrdenProcDeta" )
	Return False			
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	IF ab_mensaje THEN
		messagebox("Atención","No Existe Lote " + String(al_lote, '00000') + " " + &
								 "dentro de los lotes asignados al proceso. Ingrese otra tarja.")			
	END IF
	
	Return False		
END IF

SELECT Count( lote_codigo)
  INTO :ll_cantidad
  FROM dbo.spro_movtofrutagranenca as mge, dbo.spro_movtofrutagrandeta as mgd
 WHERE mge.mfge_numero 	= 	mgd.mfge_numero
 	AND mge.plde_codigo 	= 	mgd.plde_codigo
	AND mge.tpmv_codigo 	= 	mgd.tpmv_codigo
	AND mge.clie_codigo 	= 	mgd.clie_codigo
	AND mge.tpmv_codigo 	> 	10
	AND mge.defg_tipdoc 	= 	:ai_TipoOrd
	AND mge.defg_docrel 	= 	:ai_proceso
	AND mge.plde_codigo	=	:ai_planta
	AND mge.clie_codigo	=	:ai_cliente
	AND mgd.lote_codigo	=	:al_lote
 USING at_transaccion;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtofrutagrandeta" )
	Return False			
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	IF ab_mensaje THEN
		messagebox("Atención","El Lote " + String(al_lote, '00000') + " " + &
								 "no posee un traspaso relacionado al actual proceso, favor comprobar información. Ingrese otra tarja.")			
	END IF
	
	Return False		
END IF

Return True
end function

public function boolean validalote (integer ai_planta, integer ai_cliente, integer ai_tipoord, integer ai_proceso, long al_lote, integer ai_tipenv, integer ai_codenv, boolean ab_mensaje, transaction at_transaccion);Long	ll_cantidad

SELECT lote_codigo, lote_pltcod
INTO	:ll_cantidad, :planta
	FROM	dbo.spro_ordenprocdeta
	WHERE 	plde_codigo		=	:ai_planta
		AND	clie_codigo		=	:ai_cliente
		AND	orpr_tipord		=	:ai_TipoOrd	
		AND	orpr_numero		=	:ai_proceso
		AND 	lote_codigo		=	:al_lote
		AND	enva_tipoen 	=	:ai_tipenv
		AND	enva_codigo		=	:ai_codenv
		USING at_transaccion;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_OrdenProcDeta" )
	Return False			
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	IF ab_mensaje THEN
		messagebox("Atención","No Existe Lote " + String(al_lote, '00000') + " " + &
								 "dentro de los lotes asignados al proceso. Ingrese otra tarja.")			
	END IF
	
	Return False		
END IF


SELECT Count(lote_codigo)
  INTO :ll_cantidad
  FROM dbo.spro_movtofrutagranenca as mge, dbo.spro_movtofrutagrandeta as mgd
 WHERE mge.mfge_numero 	= 	mgd.mfge_numero
 	AND mge.plde_codigo 	= 	mgd.plde_codigo
	AND mge.tpmv_codigo 	= 	mgd.tpmv_codigo
	AND mge.clie_codigo 	= 	mgd.clie_codigo
	AND mge.tpmv_codigo 	> 	10
	AND mge.defg_tipdoc 	= 	:ai_TipoOrd
	AND mge.defg_docrel 	= 	:ai_proceso
	AND mge.plde_codigo	=	:ai_planta
	AND mge.clie_codigo	=	:ai_cliente
	AND mgd.lote_codigo	=	:al_lote
 USING at_transaccion;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtofrutagrandeta" )
	Return False			
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	IF ab_mensaje THEN
		messagebox("Atención","El Lote " + String(al_lote, '00000') + " " + &
								 "no posee un traspaso relacionado al actual proceso, favor comprobar información. "+&
								 "Ingrese otra tarja.", StopSign!)			
	END IF
	
	Return False		
END IF

Return True

Return True
end function

public subroutine controlmultiplanta (integer ai_cliente, long al_planta, long al_tarja);istr_mant.Argumento[1]	=	String(ai_cliente)
istr_mant.Argumento[2]	=	String(al_planta)
istr_mant.Argumento[3]	=	String(al_tarja)

OpenWithParm(w_popup_seleccion_tarja, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF UpperBound(istr_mant.argumento) > 4 THEN
	plantalote	=	Long(istr_mant.argumento[4])
	especie		=	Integer(istr_mant.argumento[5])
	lotetarja	=	Long(istr_mant.argumento[6])
END IF
end subroutine

public function boolean validalote (integer ai_planta, integer ai_cliente, integer ai_tipoord, integer ai_proceso, long al_lote, integer ai_tipenv, integer ai_codenv, integer ai_pltcod, boolean ab_mensaje, transaction at_transaccion);Long	ll_cantidad

SELECT lote_codigo, lote_pltcod
INTO	:ll_cantidad, :planta
	FROM	dbo.spro_ordenprocdeta
	WHERE 	plde_codigo		=	:ai_planta
		AND	clie_codigo		=	:ai_cliente
		AND	orpr_tipord		=	:ai_TipoOrd	
		AND	orpr_numero		=	:ai_proceso
		AND 	lote_codigo		=	:al_lote
		AND	enva_tipoen 	=	:ai_tipenv
		AND	enva_codigo		=	:ai_codenv
		AND   lote_pltcod		=	:ai_PltCod
		USING at_transaccion;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_OrdenProcDeta" )
	Return False			
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	IF ab_mensaje THEN
		messagebox("Atención","No Existe Lote " + String(al_lote, '00000') + " " + &
								 "dentro de los lotes asignados al proceso. Ingrese otra tarja.")			
	END IF
	
	Return False		
END IF


SELECT Count(lote_codigo)
  INTO :ll_cantidad
  FROM dbo.spro_movtofrutagranenca as mge, dbo.spro_movtofrutagrandeta as mgd
 WHERE mge.mfge_numero 	= 	mgd.mfge_numero
 	AND mge.plde_codigo 	= 	mgd.plde_codigo
	AND mge.tpmv_codigo 	= 	mgd.tpmv_codigo
	AND mge.clie_codigo 	= 	mgd.clie_codigo
	AND mge.tpmv_codigo 	> 	10
	AND mge.defg_tipdoc 	= 	:ai_TipoOrd
	AND mge.defg_docrel 	= 	:ai_proceso
	AND mge.plde_codigo	=	:ai_planta
	AND mge.clie_codigo	=	:ai_cliente
	AND mgd.lote_codigo	=	:al_lote
	AND mgd.lote_pltcod = :ai_PltCod
 USING at_transaccion;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtofrutagrandeta" )
	Return False			
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	IF ab_mensaje THEN
		messagebox("Atención","El Lote " + String(al_lote, '00000') + " " + &
								 "no posee un traspaso relacionado al actual proceso, favor comprobar información. "+&
								 "Ingrese otra tarja.", StopSign!)			
	END IF
	
	Return False		
END IF

Return True

Return True
end function

on uo_validacionesvaciado.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_validacionesvaciado.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

