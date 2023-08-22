$PBExportHeader$uo_actividadestacion.sru
$PBExportComments$Objeto de Validación de Ubicaciones
forward
global type uo_actividadestacion from nonvisualobject
end type
end forward

global type uo_actividadestacion from nonvisualobject
end type
global uo_actividadestacion uo_actividadestacion

type variables
Integer	Codigo, Planta, Activo, Tipo
String		Nombre,  Usuario, Plan
DateTime	Fecha
end variables

forward prototypes
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	ACTI_CODIGO,ACTI_NOMBRE,ACTI_TIPACT,
			ACTI_ACTIVO,ACTI_FECHAS,USUA_CODIGO,
			PCTA_NUMERO
	INTO	:Codigo, :Nombre, :Tipo,
			:Activo, :Fecha, :Usuario, :Plan
	FROM	dba.actividadestacion
	WHERE acti_codigo = :ai_Codigo
	USING	at_Transaccion;

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Actividades de Estaciones de Trabajo")
	lb_Retorno	=	False
ElseIf at_Transaccion.SQLCode = 100 Then
	lb_Retorno	=	False

	If ab_Mensaje Then
		MessageBox("Atención", "Código de Actividad (" + String(ai_Codigo, '000')  + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	End If
End If

Return lb_Retorno
end function

on uo_actividadestacion.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_actividadestacion.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

