$PBExportHeader$uo_control_historico_proceso.sru
forward
global type uo_control_historico_proceso from nonvisualobject
end type
end forward

global type uo_control_historico_proceso from nonvisualobject
end type
global uo_control_historico_proceso uo_control_historico_proceso

forward prototypes
public function boolean insertahistoria (integer ai_cliente, long al_planta, integer ai_tipord, long al_proceso, integer ai_codmod, integer ai_tipmov, date ad_fecmov, time at_hormov, string as_pcname, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean insertahistoria (integer ai_cliente, long al_planta, integer ai_tipord, long al_proceso, integer ai_codmod, integer ai_tipmov, date ad_fecmov, time at_hormov, string as_pcname, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_retorno

INSERT INTO dbo.spro_historicoprocesos  
		( clie_codigo, plde_codigo, orpr_tipord,   
		  orpr_numero, hiop_codmod, hiop_tipmod,   
		  hiop_fecmov, hiop_hormov, hiop_pcname )  
VALUES( :ai_cliente, :al_planta,  :ai_tipord,   
		  :al_proceso, :ai_codmod,  :ai_tipmov,   
		  :ad_fecmov,  :at_hormov,  :as_pcname )
USING at_transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Escritura de Tabla spro_historicoprocesos")
	IF ab_Mensaje THEN
		Messagebox("Error", "No se ha podido almacenar el movimiento~r~n"+&
								  "en el historico de proceso.")
	END IF
ELSE
	lb_Retorno	=	True
END IF

Return lb_retorno
end function

on uo_control_historico_proceso.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_control_historico_proceso.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

