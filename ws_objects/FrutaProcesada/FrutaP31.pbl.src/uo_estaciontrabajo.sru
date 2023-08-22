$PBExportHeader$uo_estaciontrabajo.sru
$PBExportComments$Objeto de Validación de Ubicaciones
forward
global type uo_estaciontrabajo from nonvisualobject
end type
end forward

global type uo_estaciontrabajo from nonvisualobject
end type
global uo_estaciontrabajo uo_estaciontrabajo

type variables
Integer	Codigo, Planta, Activo, Tipo
String		Nombre,  Usuario
DateTime	Fecha
end variables

forward prototypes
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	ESTA_CODIGO,ESTA_NOMBRE,ESTA_TIPEST,
			ESTA_ACTIVO,ESTA_FECHAC,USUA_CODIGO
	INTO	:Codigo, :Nombre, :Tipo,
			:Activo, :Fecha, :Usuario
	FROM	dbo.estaciontrabajo
	WHERE ESTA_codigo = :ai_Codigo
	USING	at_Transaccion;

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Actividades de Estaciones de Trabajo")
	lb_Retorno	=	False
ElseIf at_Transaccion.SQLCode = 100 Then
	lb_Retorno	=	False

	If ab_Mensaje Then
		MessageBox("Atención", "Código de Estacion Trabajo (" + String(ai_Codigo, '000')  + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	End If
End If

Return lb_Retorno
end function

on uo_estaciontrabajo.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_estaciontrabajo.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

