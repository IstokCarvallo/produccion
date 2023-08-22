$PBExportHeader$uo_pronconectividad.sru
$PBExportComments$Objeto de Validación de Cámaras de Frigoríficos
forward
global type uo_pronconectividad from nonvisualobject
end type
end forward

global type uo_pronconectividad from nonvisualobject
end type
global uo_pronconectividad uo_pronconectividad

type variables
Integer	Codigo, Temporada
String		Nombre, ODBC, Servidor, Base, Usuario, Password, DBMS, Ip, Puerto, Ubicacion
end variables

forward prototypes
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	prco_codigo, prco_nombre, prco_odbc, prco_servid,
			prco_baseda, prco_usuari, prco_passwo, prco_dbms,
			prco_addrip, prco_nropor, prco_ubicac, pate_tempor
	INTO	:Codigo, :Nombre, :ODBC, :Servidor, 
			:Base, :Usuario, :Password, :DBMS,
			:Ip, :Puerto, :Ubicacion, :Temporada
	FROM	dbo.pron_conexion
	WHERE	prco_codigo	=	:ai_Codigo
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Conexiones.")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código Conexion " + String(ai_Codigo, '0000') + &
						", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

on uo_pronconectividad.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_pronconectividad.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

