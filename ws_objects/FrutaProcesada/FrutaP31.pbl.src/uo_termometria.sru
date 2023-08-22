$PBExportHeader$uo_termometria.sru
$PBExportComments$Objeto de Validación de Termometria
forward
global type uo_termometria from nonvisualobject
end type
end forward

global type uo_termometria from nonvisualobject
end type
global uo_termometria uo_termometria

type variables
Integer	Planta, Camara, Proceso
Dec{2}	Temperatura
DateTime	Fecha, Desde, Hasta, Volteo
end variables

forward prototypes
public function boolean existe (long ai_planta, long ai_codigo, boolean ab_mensaje, transaction at_transaccion)
public subroutine busqueda (long ai_planta, long codigo)
public function boolean existe (long ai_planta, long ai_camara, long ai_codigo, long al_pallet, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe (long ai_planta, long ai_camara, long ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (long ai_planta, long ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
Long		ll_Count

Select  Count(teen_numero)
	Into :ll_Count
    From dbo.termometriaenca
	WHERE plde_codigo = :ai_Planta
		And teen_numero = :ai_Codigo
	USING	at_Transaccion;

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Temperaturas Pallet.")
	lb_Retorno	=	False
ElseIf at_Transaccion.SQLCode = 100 Then
	lb_Retorno	=	False

	If ab_Mensaje Then
		MessageBox("Atención", "Código de Proceso (" + String(ai_Codigo, '000')  + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	End If
Else
	If ll_Count > 1 Then
		Busqueda(ai_Planta, ai_Codigo)
	ElseIf ll_Count = 1 Then
		Select  plde_codigo, cama_codigo, teen_numero, teen_fechas,
				teen_fecini, teen_fecfin, teen_fecvol, teen_protem
		Into :Planta, :Camara, :Proceso, :Fecha,
				:Desde, :Hasta, :Volteo, :Temperatura
		 From dbo.termometriaenca
		WHERE plde_codigo = :ai_Planta
			And teen_numero = :ai_Codigo
		USING	at_Transaccion;
	Else
		lb_Retorno	=	False
	End If	
End If

Return lb_Retorno
end function

public subroutine busqueda (long ai_planta, long codigo);str_busqueda	lstr_Busq

lstr_busq.argum[1]	= String(ai_Planta)
lstr_busq.argum[2]	= String(Codigo)

OpenWithParm(w_busc_procesofrio, lstr_busq)

lstr_busq = Message.PowerObjectParm

if UpperBound(lstr_busq.Argum) < 3 Then Return

If lstr_busq.Argum[2] <> "" Then	Existe(ai_Planta, Integer(lstr_busq.argum[3]), long(lstr_busq.argum[2]), False, Sqlca)

end subroutine

public function boolean existe (long ai_planta, long ai_camara, long ai_codigo, long al_pallet, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
Long		ll_Count

Select  Count(paen_numero)
	Into :ll_Count
    From dbo.termometriadeta
	WHERE plde_codigo = :ai_Planta
		And cama_codigo = :ai_Camara
		And teen_numero = :ai_Codigo
		And paen_numero = :al_Pallet
	USING	at_Transaccion;

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Temperaturas Pallet.")
	lb_Retorno	=	False
ElseIf at_Transaccion.SQLCode = 100 Then
	lb_Retorno	=	False

	If ab_Mensaje Then
		MessageBox("Atención", "Código de Proceso (" + String(ai_Codigo, '000')  + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	End If
Else
	If ll_Count > 0 Then
		If ab_Mensaje Then
			MessageBox('Atención', 'El Numero de Pallet Indicado Existe en el Mismo Proceso/Camara (' + String(ai_Codigo, '0000') + '/' + String(ai_Camara, '0000') + ')' )			
		End If
	Else
		lb_Retorno	=	False
	End If	
End If

Return lb_Retorno
end function

public function boolean existe (long ai_planta, long ai_camara, long ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

Select  plde_codigo, cama_codigo, teen_numero, teen_fechas,
        	teen_fecini, teen_fecfin, teen_fecvol, teen_protem
	Into :Planta, :Camara, :Proceso, :Fecha,
			:Desde, :Hasta, :Volteo, :Temperatura
    From dbo.termometriaenca
	WHERE plde_codigo = :ai_Planta
		And cama_codigo = :ai_Camara
		And teen_numero = :ai_Codigo
	USING	at_Transaccion;

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Temperaturas Pallet.")
	lb_Retorno	=	False
ElseIf at_Transaccion.SQLCode = 100 Then
	lb_Retorno	=	False

	If ab_Mensaje Then
		MessageBox("Atención", "Código de Proceso (" + String(ai_Codigo, '000')  + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	End If
End If

Return lb_Retorno
end function

on uo_termometria.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_termometria.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

