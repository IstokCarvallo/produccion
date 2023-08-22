$PBExportHeader$uo_buscadatosproceso.sru
$PBExportComments$Objeto de Validación Busca datos
forward
global type uo_buscadatosproceso from nonvisualobject
end type
end forward

global type uo_buscadatosproceso from nonvisualobject
end type
global uo_buscadatosproceso uo_buscadatosproceso

type variables
Integer	Cliente, Planta, Productor, Especie, Variedad, Predio , Cuartel, Packing, &
			Etiqueta, Categoria, VarieRotula
			
Date		Fecha

String	TipoFrio
	
end variables

forward prototypes
public function boolean buscapredio (integer ai_lote, boolean ab_mensaje, transaction at_transaccion)
public function boolean buscaplanta (integer ai_planta, boolean ab_mensaje, transaction at_transaccion)
public function boolean buscacatetiq (long al_proceso, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe (long al_proceso, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente)
end prototypes

public function boolean buscapredio (integer ai_lote, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno

lb_Retorno	=	True

SELECT prbr_codpre, prcc_codigo
INTO    :Predio , :Cuartel
FROM   dbo.spro_lotesfrutagranel
WHERE lote_pltcod   = :Planta
AND     lote_espcod = :Especie
AND     lote_codigo = :ai_lote
USING  at_transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Spro_Lotesfrutagranel")
		
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Proceso " + String(ai_lote) + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean buscaplanta (integer ai_planta, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	

lb_Retorno	=	True

SELECT  prpa_packin 
	INTO     :Packing
	FROM    dbo.spro_paramplanta
	WHERE  plde_codigo = :ai_Planta
	USING	at_Transaccion; 

	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Spro_Lotesfrutagranel")
		
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Proceso " + String(ai_Planta) + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean buscacatetiq (long al_proceso, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno
Integer	li_Existe

lb_Retorno	=	True

SELECT MAX(refe_secuen)
	INTO :li_Existe
	FROM	dbo.spro_resulfrutemba
	WHERE	 	plde_codigo	=	:Planta	
		AND 	orpr_tipdoc	=	4
		AND	orpr_numero	=	:al_proceso
		AND	prod_codigo	=	:Productor 
		AND	espe_codigo	=	:Especie	
		AND	vari_codigo	=	:Variedad		 
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Spro_Ordenproceso")
		
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Proceso " + String(al_Proceso) + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
		lb_Retorno	=	False					
	END IF
ELSEIF li_Existe	>	0	THEN
	
		SELECT cate_codigo,etiq_codigo
			INTO :Categoria, :Etiqueta
			FROM	dbo.spro_resulfrutemba
			WHERE	 	plde_codigo	=	:Planta	
				AND 	orpr_tipdoc	=	4
				AND	orpr_numero	=	:al_proceso
				AND	prod_codigo	=	:Productor 
				AND	espe_codigo	=	:Especie	
				AND	vari_codigo	=	:Variedad		 
				AND	refe_secuen	=	:li_Existe
			USING	at_Transaccion;
	
		IF at_Transaccion.SQLCode = 100 THEN
			IF ab_Mensaje THEN
				MessageBox("Atención", "Código de Proceso " + String(al_Proceso) + &
							", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
					lb_Retorno	=	False
			END IF
		END IF		
END IF

RETURN lb_Retorno

end function

public function boolean existe (long al_proceso, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente);Boolean	lb_Retorno	
Integer	li_VarieRotula, li_frio

cliente = ai_cliente

lb_Retorno	=	True

SELECT	plde_codigo,	orpr_fecpro,	prod_codigo,
			espe_codigo,	vari_codigo, 	etiq_codigo, 
			frio_tipofr
	INTO 	:Planta, 		:Fecha, 			:Productor, 
			:Especie, 		:Variedad, 		:Etiqueta,
			:TipoFrio
	FROM	dbo.spro_ordenproceso
	WHERE orpr_numero	=	:al_proceso
	  and orpr_tipord = 	4
	  and clie_codigo =	:ai_cliente
	  and plde_codigo =	:gstr_paramplanta.codigoplanta
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Spro_Ordenproceso")
		
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Proceso " + String(al_Proceso) + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
		lb_Retorno	=	False
	END IF
END IF

SELECT vari_relaci 
   INTO :VarieRotula
	FROM dbo.variedades
	WHERE espe_codigo = :Especie
	AND   vari_codigo = :Variedad
	USING	at_Transaccion;
	
IF IsNull(VarieRotula) OR VarieRotula = 0 THEN
	VarieRotula = Variedad
END IF

RETURN lb_Retorno
end function

on uo_buscadatosproceso.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_buscadatosproceso.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

