$PBExportHeader$uo_transportista.sru
$PBExportComments$Objeto de Validación de Transportista
forward
global type uo_transportista from nonvisualobject
end type
end forward

global type uo_transportista from nonvisualobject
end type
global uo_transportista uo_transportista

type variables
Integer	Codigo,CodCli,Comuna
String	Nombre, Abrevi,CodMat,Direcc,Ciud,Direccion,NroRut,Telefono


end variables

forward prototypes
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

  SELECT tran_codigo,tran_nombre,   
         tran_direcc,tran_ciudad  
    INTO :Codigo,:Nombre,   
         :Direcc,:Ciud  
    FROM dbo.transportista  
   WHERE tran_codigo = :ai_codigo
    USING	at_Transaccion;
	 
SELECT	tran_codigo,tran_nombre,tran_direcc,tran_nrotel,tran_nrorut//,comu_codigo
	INTO	:Codigo,:Nombre,: Direccion,:NroRut,:Telefono//,:Comuna
	FROM	dbo.transportista
	WHERE	tran_codigo	=	:ai_codigo
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Transportista")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Transportista " + String(ai_Codigo) + &
					", no ha sido Ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_transportista.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_transportista.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

