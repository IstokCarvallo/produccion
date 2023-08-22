$PBExportHeader$uo_analizapallet.sru
forward
global type uo_analizapallet from nonvisualobject
end type
end forward

global type uo_analizapallet from nonvisualobject
end type
global uo_analizapallet uo_analizapallet

type variables
Integer	cliente, especie, Variedad
Long		NroPallet, Cajas, Productor
String		CodBarra, Embalaje, Calibre, NomEspecie, NomVariedad, ProdNombre
Date		Fecha
end variables

forward prototypes
public function boolean existe_especie (integer ai_especie, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe_calibre (string as_calibre, integer ai_especie, integer ai_variedad, readonly boolean ab_mensaje, transaction at_transaccion)
public function boolean existe_productor (long al_productor, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe_embalaje (string as_embalaje, integer ai_cliente, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe_variedad (integer ai_especie, integer ai_variedad, boolean ab_mensaje, transaction at_transaccion)
public function boolean descompone_codigo (string as_data, integer ai_cliente, transaction at_transaccion)
public function boolean analiza_datos (string as_dato, transaction at_transaccion)
end prototypes

public function boolean existe_especie (integer ai_especie, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	espe_codigo,espe_nombre
	INTO	:Especie, :NomEspecie
	FROM	dbo.especies
	WHERE	espe_codigo	=	:ai_especie
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Especies")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Especie " + String(Especie, '00') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existe_calibre (string as_calibre, integer ai_especie, integer ai_variedad, readonly boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	vaca_calibr
	INTO	:Calibre
	FROM	dbo.variecalibre
	WHERE	vaca_calibr	=	:as_calibre
	AND	espe_codigo =	:ai_especie
	AND	vari_codigo = 	:ai_variedad
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla variecalibre")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Calibre " + String(calibre) + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existe_productor (long al_productor, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	prod_codigo,prod_nombre
	INTO	:Productor,:ProdNombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:al_productor
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Productores")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Productor " + String(Productor) + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existe_embalaje (string as_embalaje, integer ai_cliente, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	emba_codigo
	INTO	:embalaje
	FROM	dbo.embalajesprod
	WHERE	emba_codigo	=	:as_embalaje
	AND	clie_codigo = 	:ai_cliente
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Embalajesprod")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Embalaje " + String(embalaje) + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existe_variedad (integer ai_especie, integer ai_variedad, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	vari_codigo,vari_nombre
	INTO	:Variedad, :NomVariedad
	FROM	dbo.variedades
	WHERE	espe_codigo	=	:ai_especie
	AND	vari_codigo = 	:ai_variedad
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Variedades")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Variedad " + String(Variedad, '0000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean descompone_codigo (string as_data, integer ai_cliente, transaction at_transaccion);string	ls_digito, ls_pallet

IF Len(as_data) < 48  THEN
	MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", &
					StopSign!)
	RETURN False
	
END IF

Especie 	= 	Integer(mid(as_data,31,2))

IF NOT existe_especie(Especie,True, at_transaccion) THEN
	Return False
END IF	

Variedad	=	Integer(mid(as_data,33,4))

IF NOT existe_variedad(Especie,Variedad,True, at_transaccion) THEN
	Return False
END IF	

Cajas		=	Long(mid(as_data,46,3))
Embalaje = 	mid(as_data,37,4)

IF NOT existe_embalaje(Embalaje,ai_cliente,True, at_transaccion) THEN
	Return False
END IF	

Calibre	=	mid(as_data,41,3)

IF NOT existe_calibre(calibre,Especie,variedad,True, at_transaccion) THEN
	Return False
END IF	

Productor=	Long(mid(as_data,25,4))

IF NOT existe_productor(productor,True, at_transaccion) THEN
	Return False
END IF

Fecha		=	Date((mid(as_data,23,2))+'-'+(mid(as_data,21,2))+'-20'+(mid(as_data,19,2)))


Return True
end function

public function boolean analiza_datos (string as_dato, transaction at_transaccion);string	ls_digito, ls_pallet, ls_verIficador, ls_rotulado
Integer	li_resultado

If Len(as_dato) < 9  Then
	MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", StopSign!)
	RETURN False
End If

If Len(as_dato) = 10 Then
	ls_rotulado	=	Mid(as_dato, 1, Len(as_dato) - 7)
	
	SELECT clie_codigo 
	  INTO :cliente
	  FROM dbo.clientesprod
	 WHERE clie_rotula = :ls_rotulado;
	
	If IsNull(cliente) OR cliente = 0  Then cliente	=	gi_CodExport
	
	NroPallet	=	Long(Mid(as_dato, Len(as_dato) - 6))
	ls_pallet	=  String(Right(as_dato, 6))
	ls_digito 	= 	f_calcula_digito(string(ls_pallet),ls_rotulado)
	CodBarra		=  Mid('00' + gs_pfijopallet + ls_rotulado + string(ls_pallet, '000000') + ls_digito, 1, 20)
	
	Return	True
ElseIf Len(as_dato) = 9 Then	
	ls_rotulado	=	Left(as_dato, 3)
	
	SELECT clie_codigo 
	  INTO :cliente
	  FROM dbo.clientesprod
	 WHERE clie_rotula = :ls_rotulado;
	 
	If IsNull(cliente) OR cliente = 0 Then cliente	=	gi_CodExport
	 
	NroPallet	=	Long(Right(as_dato, 6))
	ls_digito	= 	f_calcula_digito(string(NroPallet),ls_rotulado)
	CodBarra		=  Mid('00' + gs_pfijopallet + as_dato + ls_digito, 1, 20)
	
	Return True
ElseIf Len(as_dato) > 15 Then
	ls_rotulado	=	mid(as_dato, 11,3)
	SELECT clie_codigo
	  INTO :cliente
	  FROM dbo.clientesprod
	 WHERE clie_rotula = :ls_rotulado;
	
	 If IsNull(cliente) OR cliente = 0  Then cliente	=	gi_CodExport

	NroPallet			=	Long(mid(as_dato, 14,6))
	ls_pallet			=	(mid(as_dato, 14,6))
	ls_digito 			= 	f_calcula_digito(string(ls_pallet),ls_rotulado)	
	ls_verIficador	= 	Right(as_dato, 1)
	
	If ls_verIficador <> ls_digito Then
		li_resultado = MessageBox("Atención",'Dígito VerIficador NO Corresponde, ~rDesea Continuar', Exclamation!, OKCancel!, 2)		
		If li_resultado <> 1 Then Return	False
	End If	
	CodBarra		=  Mid(as_dato, 1, 20)
	Return True
End If

Return False
end function

on uo_analizapallet.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_analizapallet.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

