$PBExportHeader$uo_existencia.sru
$PBExportComments$Objeto de Validación con existencia
forward
global type uo_existencia from nonvisualobject
end type
end forward

global type uo_existencia from nonvisualobject
end type
global uo_existencia uo_existencia

type variables
Integer	Codigo,kildec,bodvirtual,bodzonal, BodDestino, DevZon
String	Nombre, Nombre_ingles, CodAra, CodSAG, Prod,itgene,prdgen, Correo, correozonal
Long		Count, numero
Date		Mesproceso
end variables

forward prototypes
public function boolean paramexistencia (boolean ab_mensaje, transaction at_transaccion)
public function boolean correlativobode (integer ai_tipdoc, integer ai_bodvirtual, integer ai_bodzonal, boolean ab_mensaje, transaction at_transaccion)
public function boolean existeencabezado (long al_docrel, integer ai_bodvirtual, integer ai_estado, integer ai_tipdoc, boolean ab_mensaje, transaction at_transaccion)
public function boolean existecliente (string as_rut, boolean ab_mensaje, transaction at_transaccion)
public subroutine numerro ()
public function boolean numeromaximo (integer ai_tipdoc, integer ai_bodvirtual, integer al_docrel, integer ai_estado, boolean ab_mesaje, transaction at_transaccion)
public function boolean actualizaexistencia (integer ai_estado, integer ai_tipdoc, integer ai_bodevirtual, long al_numero, integer al_docrel, boolean ab_mensaje, transaction at_transaccion)
public function boolean nummaxpbodega (integer ai_tipdoc, integer ai_bodzonal, integer ai_bodvirtual, long al_docrel, boolean ab_mensaje, transaction at_transaccion)
public function boolean bodega_zonal (integer ai_bodega, transaction at_transaccion, boolean ab_retorno)
public function boolean existecliente2 (string as_rut, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean paramexistencia (boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

// DevZon	=	Controla si al hacer una salida el material que se rebaja de la bodega comercial retorna
//   				o no a la bodega zonal.

SELECT expa_pacfri, expa_bodega, expa_itgene, expa_prdgen, expa_pakcom, isnull(expa_devzon,0), expa_mespro, expa_correo
	INTO :bodvirtual, :bodzonal, :itgene, :prdgen, :BodDestino, :DevZon, :Mesproceso, :correo
	FROM dba.existeparam
	USING at_transaccion;
	
IF at_transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"existeparam")
	lb_Retorno = True
END IF

Return lb_Retorno
end function

public function boolean correlativobode (integer ai_tipdoc, integer ai_bodvirtual, integer ai_bodzonal, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
Integer	li_Ultimo
String	ls_nombrepc

SELECT max(cobo_actual)
	INTO :numero
	FROM dba.correlbode
	WHERE mden_tipdoc = :ai_tipdoc
	AND bode_codigo 	= :ai_bodvirtual
	AND cobo_bodadm 	= :ai_bodzonal
	USING	at_transaccion;
		
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Correlativos de Bodega")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 AND ab_Mensaje THEN

	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean existeencabezado (long al_docrel, integer ai_bodvirtual, integer ai_estado, integer ai_tipdoc, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT count() 
	INTO	:count
	FROM 	dba.exismovtoenca
	WHERE mden_docrel =:al_docrel
	AND	bode_codigo =:ai_bodvirtual
	AND	mden_estado =:ai_estado
	AND	mden_tipdoc =:ai_tipdoc
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla exismovtoenca")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

END IF

RETURN lb_Retorno
end function

public function boolean existecliente (string as_rut, boolean ab_mensaje, transaction at_transaccion);Boolean lb_retorno = True

SELECT clpr_rut
	INTO	:prod
	FROM	dba.clienprove
	WHERE clpr_rut = :as_rut
	USING at_transaccion; 
	
IF at_transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"clienprove")
	lb_retorno = False
END IF		

Return lb_retorno	
end function

public subroutine numerro ();
end subroutine

public function boolean numeromaximo (integer ai_tipdoc, integer ai_bodvirtual, integer al_docrel, integer ai_estado, boolean ab_mesaje, transaction at_transaccion);Boolean	lb_retorno = True

SELECT MAX(mden_numero)
	INTO :numero
	FROM dba.exismovtoenca
	WHERE mden_tipdoc = :ai_tipdoc
	AND bode_codigo = :ai_bodvirtual
	AND mden_docrel = :al_docrel
	AND mden_estado = :ai_estado
	USING at_transaccion;
	
IF at_transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"exismovtoenca")
	lb_retorno = False
END IF

Return lb_retorno
	
end function

public function boolean actualizaexistencia (integer ai_estado, integer ai_tipdoc, integer ai_bodevirtual, long al_numero, integer al_docrel, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_retorno = True
Date		ld_fecmov

IF NOT paramexistencia(False,at_transaccion) THEN
	F_ErrorBaseDatos(at_transaccion,"existeparam")
	Message.DoubleParm = -1
	Return False
END IF	

SELECT mden_fecmov
INTO :ld_fecmov
FROM dba.exismovtoenca
WHERE mden_tipdoc = :ai_tipdoc
	AND bode_codigo = :ai_bodevirtual
	AND mden_numero = :al_numero
	AND mden_docrel = :al_docrel
	USING at_transaccion;
	
IF at_transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"exismovtoenca")
	Message.DoubleParm = -1
	lb_retorno = False
END IF		
	
IF Mesproceso > ld_fecmov THEN
	Message.DoubleParm = -1
	RETURN False
END IF

UPDATE dba.exismovtoenca 
SET mden_estado = :ai_estado
	WHERE mden_tipdoc = :ai_tipdoc
	AND bode_codigo = :ai_bodevirtual
	AND mden_numero = :al_numero
	AND mden_docrel = :al_docrel
	USING at_transaccion;
		
IF at_transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"exismovtoenca")
	Message.DoubleParm = -1
	lb_retorno = False
END IF	

Return lb_retorno
end function

public function boolean nummaxpbodega (integer ai_tipdoc, integer ai_bodzonal, integer ai_bodvirtual, long al_docrel, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_retorno = True

SELECT MAX(mden_numero)
	INTO :numero
	FROM dba.exismovtoenca
	WHERE mden_tipdoc = :ai_tipdoc
	AND bode_codigo = :ai_bodzonal
	AND mden_bodest = :ai_bodvirtual
	AND mden_docrel = :al_docrel
	USING at_transaccion;
	
IF at_transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"exismovtoenca")
	lb_retorno = False
END IF

Return lb_retorno
	
end function

public function boolean bodega_zonal (integer ai_bodega, transaction at_transaccion, boolean ab_retorno);Boolean lb_retorno = True

SELECT bode_correo
	INTO	:correozonal
	FROM	dba.bodegas
	WHERE bode_codigo = :ai_bodega
	USING at_transaccion; 
	
IF at_transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"Bodegas")
	lb_retorno = False
END IF		

Return lb_retorno	
end function

public function boolean existecliente2 (string as_rut, boolean ab_mensaje, transaction at_transaccion);Boolean lb_retorno = True

SELECT clpr_rut
	INTO	:prod
	FROM	dba.clienprove
	WHERE clpr_nrorut = :as_rut
	USING at_transaccion; 
	
IF at_transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_transaccion,"clienprove")
	lb_retorno = False
END IF		

Return lb_retorno	
end function

on uo_existencia.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_existencia.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

