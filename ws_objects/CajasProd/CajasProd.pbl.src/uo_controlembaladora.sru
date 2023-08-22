$PBExportHeader$uo_controlembaladora.sru
forward
global type uo_controlembaladora from nonvisualobject
end type
end forward

global type uo_controlembaladora from nonvisualobject
end type
global uo_controlembaladora uo_controlembaladora

type variables
integer	cliente, planta
Long		embaladora, correlativo
Date		fecha
end variables

forward prototypes
public function boolean existe (integer ai_cliente, integer ai_planta, long al_embaladora, long al_correlativo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_cliente, integer ai_planta, long al_embaladora, long al_correlativo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_retorno = True

  SELECT clie_codigo, plde_codigo,cpco_embala,cpco_fecemb,cpco_numero
    INTO :cliente, :planta, :embaladora, :fecha, :correlativo
    FROM dbo.spro_cajasprodcontrol
	WHERE clie_codigo = :ai_cliente
	  AND plde_codigo = :ai_planta
	  AND cpco_embala = :al_embaladora
	  AND cpco_numero = :al_correlativo
	USING at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Control")
	
ELSEIF at_Transaccion.SQLCode <> 100 THEN
	lb_Retorno	=	False
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código Correlativo de Embaladora" + &
						String(al_embaladora, '00000') + '-' + &
						String(al_correlativo, '00000') + &
					  ", ya ha sido ingresado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

on uo_controlembaladora.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_controlembaladora.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

