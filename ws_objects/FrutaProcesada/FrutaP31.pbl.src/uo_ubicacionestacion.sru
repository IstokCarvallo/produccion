$PBExportHeader$uo_ubicacionestacion.sru
$PBExportComments$Objeto de Validación de Ubicaciones
forward
global type uo_ubicacionestacion from nonvisualobject
end type
end forward

global type uo_ubicacionestacion from nonvisualobject
end type
global uo_ubicacionestacion uo_ubicacionestacion

type variables
Integer	Codigo, Planta, Activo
String		Nombre,  Usuario
DateTime	Fecha
end variables

forward prototypes
public function boolean existe (integer ai_planta, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_planta, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	UBIC_CODIGO, PLDE_CODIGO, UBIC_NOMBRE, 
			UBIC_FECHAC, UBIC_ACTIVO, USUA_CODIGO
	INTO	:Codigo, :Planta, :Nombre, 
			:Fecha, :Activo, :Usuario
	FROM	dba.ubicacionestacion
	WHERE plde_codigo =	:ai_Planta
		And ubic_codigo = :ai_Codigo
	USING	at_Transaccion;

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Ubicaciones de Estaciones de Trabajo")
	lb_Retorno	=	False
ElseIf at_Transaccion.SQLCode = 100 Then
	lb_Retorno	=	False

	If ab_Mensaje Then
		MessageBox("Atención", "Código de Ubicacion (" + String(ai_Codigo, '00') + " - Para Planta Codigo: " + &
						String(ai_Planta, '0000') + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	End If
End If

Return lb_Retorno
end function

on uo_ubicacionestacion.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_ubicacionestacion.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

