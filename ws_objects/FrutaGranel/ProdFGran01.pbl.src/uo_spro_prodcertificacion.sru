$PBExportHeader$uo_spro_prodcertificacion.sru
$PBExportComments$Objeto de Validación de Situaciones de Bitácora de Movimientos
forward
global type uo_spro_prodcertificacion from nonvisualobject
end type
end forward

global type uo_spro_prodcertificacion from nonvisualobject
end type
global uo_spro_prodcertificacion uo_spro_prodcertificacion

type variables
Long		prod_codigo
Integer	prpr_codigo, espe_codigo, nice_codigo
Decimal	prec_codigo
Date		prce_fecaud, prce_feccer, prce_fecexp
end variables

forward prototypes
public function string rotulocert (boolean ab_mensaje, transaction at_transaccion)
public function boolean existe (long al_prod, integer ai_predio, integer ai_espe, decimal ai_nice, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function string rotulocert (boolean ab_mensaje, transaction at_transaccion);String prec_nombre


select prot_nombre into :prec_nombre
		from dbo.cert_protocolo
		 where prot_codigo = :prec_codigo
		using at_transaccion;
  
 

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Rotulos de Certificaciones")

	prec_nombre	=	''

ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Rotulo de Certificación " + String(prec_codigo, '000') + &
					", no ha sido registrado.")
	END IF
	prec_nombre	=	''
END IF

RETURN prec_nombre
end function

public function boolean existe (long al_prod, integer ai_predio, integer ai_espe, decimal ai_nice, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

select distinct prot_codigo into :prec_codigo
  from dbo.cert_certificacion_prod
 where "prod_codigo"	=	:al_prod
 	and "prpr_codigo"	=	:ai_predio
	and "espe_codigo"	=	:ai_espe
	and "prot_codigo"	=	:ai_nice
 USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Certificaciones")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código Certificación " + String(ai_Nice, '00') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on uo_spro_prodcertificacion.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_spro_prodcertificacion.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

