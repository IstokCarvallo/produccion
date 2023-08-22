$PBExportHeader$uo_spro_ordenproceso.sru
$PBExportComments$Objeto de Validación de Existencia de Orden de Proceso.
forward
global type uo_spro_ordenproceso from nonvisualobject
end type
end forward

global type uo_spro_ordenproceso from nonvisualobject
end type
global uo_spro_ordenproceso uo_spro_ordenproceso

type variables
Integer	Planta, TipoOrden, Especie, Variedad, PeriodoFrio, Linea, &
			Turno, Estado, Cliente, VarRot, SecuenciaMax
Long		NumeroOrden, Productor
String	TipoFrio, NombreTipoFrio, NombreProductor, NombreEspecie, &
			NombreVariedad, NombrePeriodoFrio
Date		FechaOrden
end variables

forward prototypes
public subroutine capturareferencias (integer ai_leeproductor, integer ai_leeespecie, integer ai_leevariedad, integer ai_leetipofrio, integer ai_leeperiodo, boolean ab_mensaje, transaction at_transaccion)
public function boolean existefechaproceso (integer ai_planta, integer ai_tipoorden, datetime ad_fecha, boolean ab_mensaje, transaction at_transaccion)
public function long maximonumero (integer ai_planta, integer ai_tipoorden, transaction at_transaccion, integer ai_cliente)
public function boolean existe (integer ai_planta, integer ai_tipoorden, long al_numero, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente)
public subroutine cargarefreproceso (integer ai_cliente, transaction at_transaccion)
public function boolean cargamaximocorrel (integer ai_planta, integer ai_tipoorden, long al_numero, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente)
public function boolean procesoenfecha (integer ai_planta, integer ai_tipoorden, date ad_fecha, boolean ab_mensaje, transaction at_transaccion)
public function boolean buscalotecomercial (transaction at_transaccion, boolean ab_mensaje)
end prototypes

public subroutine capturareferencias (integer ai_leeproductor, integer ai_leeespecie, integer ai_leevariedad, integer ai_leetipofrio, integer ai_leeperiodo, boolean ab_mensaje, transaction at_transaccion);IF ai_LeeProductor = 1 THEN
	SELECT prod_nombre
	INTO	:NombreProductor
	FROM	dbo.productores
	WHERE	prod_codigo = :Productor;

	IF at_Transaccion.SQLCode = -1 AND ab_Mensaje THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Productores")
	END IF
END IF

IF ai_LeeEspecie = 1 THEN
	SELECT espe_nombre
	INTO	:NombreEspecie
	FROM	dbo.especies
	WHERE	espe_codigo = :Especie;

	IF at_Transaccion.SQLCode = -1 AND ab_Mensaje THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Especies")
	END IF
END IF

IF ai_LeeVariedad = 1 THEN
	SELECT vari_nombre
	INTO	:NombreVariedad
	FROM	dbo.variedades
	WHERE	espe_codigo = :Especie
	AND	vari_codigo = :Variedad;

	IF at_Transaccion.SQLCode = -1 AND ab_Mensaje THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Variedades")
	END IF
END IF

IF ai_LeeTipoFrio = 1 THEN
	SELECT frio_nombre
	INTO	:NombreTipoFrio
	FROM	dbo.tratamientofrio
	WHERE	frio_tipofr = :TipoFrio;
	IF at_Transaccion.SQLCode = -1 AND ab_Mensaje THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Tipo de Frío")
	END IF
END IF

IF ai_LeePeriodo = 1 THEN
	SELECT pefr_nombre
	INTO	:NombrePeriodoFrio
	FROM	dbo.periodofrio
	WHERE	pefr_codigo = :PeriodoFrio;
	IF at_Transaccion.SQLCode = -1 AND ab_Mensaje THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Periodo Frío")
	END IF
END IF
end subroutine

public function boolean existefechaproceso (integer ai_planta, integer ai_tipoorden, datetime ad_fecha, boolean ab_mensaje, transaction at_transaccion);Integer	li_Proceso
Boolean	lb_Retorno = True

SELECT	Count(plde_codigo)
	INTO	:li_Proceso
	FROM	dbo.spro_ordenproceso
	WHERE	plde_codigo 	=	:ai_Planta
	AND	:ai_TipoOrden 	in	(orpr_tipord, -1)
	AND	orpr_fecpro 	=	:ad_Fecha
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Ordenes de Proceso")

	lb_Retorno	=	False
ELSEIF li_Proceso = 0 THEN
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function long maximonumero (integer ai_planta, integer ai_tipoorden, transaction at_transaccion, integer ai_cliente);Long	ll_NumeroMaximo

SELECT	IsNull(Max(orpr_numero), 0) + 1
	INTO	:ll_NumeroMaximo
	FROM	dbo.spro_ordenproceso
	WHERE	plde_codigo	=	:ai_Planta
	and 	clie_codigo	=	:ai_cliente
	AND	orpr_tipord	=	:ai_TipoOrden ;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Orden de Proceso")

ELSE
	RETURN ll_NumeroMaximo
END IF

end function

public function boolean existe (integer ai_planta, integer ai_tipoorden, long al_numero, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente);Boolean	lb_Retorno = True

Cliente = ai_Cliente

	SELECT	plde_codigo, orpr_tipord, orpr_numero, orpr_fecpro, prod_codigo, 
				espe_codigo, vari_codigo, frio_tipofr, pefr_codigo, line_codigo, 
				orpr_nrotur, orpr_estado
		INTO	:Planta, :TipoOrden, :NumeroOrden, :FechaOrden, :Productor, :Especie,
				:Variedad, :TipoFrio, :PeriodoFrio, :Linea, :Turno, :Estado
		FROM	dbo.spro_ordenproceso
		WHERE	plde_codigo =	:ai_Planta
		AND	orpr_tipord	=	:ai_TipoOrden
		AND	orpr_numero =	:al_Numero
		AND   clie_codigo =  :ai_cliente
		USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Ordenes de Proceso")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	 IF ab_Mensaje THEN
		MessageBox("Atención", "Orden de Proceso " + String(al_Numero, '00000000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Número.")
	 END IF
	lb_Retorno	=	False
END IF

	SELECT vari_relaci 
	  INTO :VarRot
	  FROM dbo.variedades
	 WHERE espe_codigo = :Especie
	 	AND vari_codigo = :Variedad;

	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Variedades")
	
		lb_Retorno	=	False
	ELSEIF at_Transaccion.SQLCode = 100 THEN
		 IF ab_Mensaje THEN
			MessageBox("Atención", "No se encuentra la Variedad indicada para este proceso.")
		 END IF
		lb_Retorno	=	False
	END IF

RETURN lb_Retorno
end function

public subroutine cargarefreproceso (integer ai_cliente, transaction at_transaccion);SELECT dbo.spro_lotesfrutacomdeta.prod_codigo, dbo.spro_lotesfrutacomdeta.vari_codigo,
		dbo.spro_lotesfrutacomdeta.frio_tipofr, dbo.spro_lotesfrutacomdeta.pefr_codigo, 1
INTO	:Productor, :Variedad, 
		:TipoFrio, :PeriodoFrio, :Turno
FROM  dbo.spro_doctointernopack,   
		dbo.spro_lotesfrutacomdeta,   
		dbo.spro_movtofrutacomenca,   
		dbo.spro_movtofrutacomdeta  
WHERE ( dbo.spro_movtofrutacomenca.clie_codigo 	= 	dbo.spro_movtofrutacomdeta.clie_codigo ) and  
		( dbo.spro_movtofrutacomenca.plde_codigo 	= 	dbo.spro_movtofrutacomdeta.plde_codigo ) and  
		( dbo.spro_movtofrutacomenca.tpmv_codigo 	= 	dbo.spro_movtofrutacomdeta.tpmv_codigo ) and  
		( dbo.spro_movtofrutacomenca.mfco_numero 	= 	dbo.spro_movtofrutacomdeta.mfco_numero ) and  
		( dbo.spro_doctointernopack.plde_codigo 	= 	dbo.spro_movtofrutacomenca.plde_codigo ) and  
		( dbo.spro_doctointernopack.dinp_tipdoc 	= 	dbo.spro_movtofrutacomenca.mfco_tipdoc ) and  
		( dbo.spro_doctointernopack.dinp_numero 	= 	dbo.spro_movtofrutacomenca.mfco_docrel ) and  
		( dbo.spro_doctointernopack.clie_codigo 	= 	dbo.spro_movtofrutacomenca.clie_codigo ) and  
		( dbo.spro_movtofrutacomdeta.lofc_pltcod 	= 	dbo.spro_lotesfrutacomdeta.lofc_pltcod ) and  
		( dbo.spro_movtofrutacomdeta.lofc_espcod 	= 	dbo.spro_lotesfrutacomdeta.lofc_espcod ) and  
		( dbo.spro_movtofrutacomdeta.lofc_lotefc 	= 	dbo.spro_lotesfrutacomdeta.lofc_lotefc ) and  
		( dbo.spro_movtofrutacomdeta.lfcd_secuen 	= 	dbo.spro_lotesfrutacomdeta.lfcd_secuen )
AND	( dbo.spro_doctointernopack.plde_codigo 	=	:Planta )
AND	( dbo.spro_doctointernopack.dinp_tipdoc	=	:TipoOrden )
AND	( dbo.spro_doctointernopack.dinp_numero 	=	:NumeroOrden )
AND 	( dbo.spro_doctointernopack.clie_codigo 	= 	:ai_cliente )
USING at_Transaccion;	


end subroutine

public function boolean cargamaximocorrel (integer ai_planta, integer ai_tipoorden, long al_numero, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente);Boolean	lb_Retorno = True

SELECT	max(orpd_secuen)
	INTO	:SecuenciaMax
	FROM	dbo.spro_ordenprocdeta
	WHERE	plde_codigo =	:ai_Planta
	AND	orpr_tipord	=	:ai_TipoOrden
	AND	orpr_numero =	:al_Numero
	AND   clie_codigo =  :ai_cliente
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Ordenes de Proceso")

	lb_Retorno	=	False
ELSE
	IF IsNull(SecuenciaMax) THEN SecuenciaMax = 0
	SecuenciaMax	=	SecuenciaMax	+	1
END IF

RETURN lb_Retorno
end function

public function boolean procesoenfecha (integer ai_planta, integer ai_tipoorden, date ad_fecha, boolean ab_mensaje, transaction at_transaccion);Integer	li_Proceso
Boolean	lb_Retorno = True

SELECT	Count(plde_codigo)
	INTO	:li_Proceso
	FROM	dbo.spro_ordenproceso
	WHERE	plde_codigo 	=	:ai_Planta
	AND	:ai_TipoOrden 	in	(orpr_tipord, -1)
	AND	orpr_fecpro 	=	:ad_Fecha
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Ordenes de Proceso")

	lb_Retorno	=	False
ELSEIF li_Proceso > 0 THEN
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean buscalotecomercial (transaction at_transaccion, boolean ab_mensaje);Integer	li_cuenta
Boolean	lb_retorno	=	True

select IsNull(Max(lfcd_secuen), -1) 
  into :li_cuenta
  from dbo.spro_lotesfrutacomdeta
 where lfcd_docrel	=	:NumeroOrden
   and lfcd_tipdoc 	= 	:TipoOrden
 Using at_Transaccion;
  
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Detalle de Lotes Comerciales")

	lb_Retorno	=	False
ELSEIF IsNull(li_cuenta) OR li_cuenta = -1 THEN
	 IF ab_Mensaje THEN
		MessageBox("Atención", "Orden de Proceso " + String(NumeroOrden, '00000000') + &
					", no tiene Lote Comercial registrado.~r~rIngrese o seleccione otro Número.")
	 END IF
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on uo_spro_ordenproceso.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_spro_ordenproceso.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

