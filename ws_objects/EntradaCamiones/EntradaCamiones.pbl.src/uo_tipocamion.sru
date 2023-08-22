$PBExportHeader$uo_tipocamion.sru
$PBExportComments$Objeto de Validación de tipo de camion
forward
global type uo_tipocamion from nonvisualobject
end type
end forward

global type uo_tipocamion from nonvisualobject
end type
global uo_tipocamion uo_tipocamion

type variables
String	Codigo,Nombre, Abrevi,CodMat
end variables

forward prototypes
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

  SELECT tica_codigo,   
         tica_nombre  
    INTO :codigo,   
         :nombre  
    FROM dbo.tipocamion  
   WHERE tica_codigo = :ai_codigo   
    USING at_Transaccion;

	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Tipo Camión")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Tipo Camión " + String(ai_Codigo) + &
					", no ha sido Ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_tipocamion.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_tipocamion.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

