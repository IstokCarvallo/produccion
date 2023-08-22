$PBExportHeader$uo_controlventanas.sru
forward
global type uo_controlventanas from nonvisualobject
end type
end forward

global type uo_controlventanas from nonvisualobject
end type
global uo_controlventanas uo_controlventanas

type variables
Private Constant	String	_URL = 'https://rioblanco-api-packing.azurewebsites.net/'
end variables
forward prototypes
public function boolean actualizacontrol (integer ai_cliente, long al_planta, long al_pallet, integer ai_operacion, integer ai_impresiones, long al_proceso)
public function boolean existe (integer ai_cliente, long al_planta, long al_pallet, integer ai_operacion, long al_proceso, integer ai_cajas)
public function boolean validarango (integer ai_cliente, long al_planta, long al_pallet, integer ai_operacion, integer ai_impresiones, long al_proceso, integer ai_cajas)
public subroutine insertacontrol (integer ai_cliente, long al_planta, long al_pallet, integer ai_cajas, long al_proceso)
public function boolean valida_inspeccion (long al_pallet, integer ai_cliente, integer ai_planta)
public function boolean actualizacorrelativo (integer ai_cliente, long al_planta, long al_pallet)
public function long correlativo (integer ai_cliente, long al_planta)
public function long correlativo (integer ai_cliente, long al_planta, integer ai_tipo)
public function boolean validarango (integer ai_cliente, long al_planta, long al_pallet, integer ai_tipo, boolean mensaje, transaction transaccion)
public function boolean actualizacorrelativo (integer ai_cliente, long al_planta, long al_pallet, integer ai_tipo)
public function boolean of_disponible (integer ai_cliente, long al_planta, integer ai_tipo, transaction transaccion)
public function boolean of_existe (integer cliente, long planta, integer tipo, boolean mensaje, transaction transaccion)
public function boolean of_actualiza (long cliente, long planta, integer tipo, ref datawindow adw)
public function boolean of_validarango (integer cliente, long planta, integer tipo, long correlativo, boolean mensaje, transaction transaccion)
end prototypes

public function boolean actualizacontrol (integer ai_cliente, long al_planta, long al_pallet, integer ai_operacion, integer ai_impresiones, long al_proceso);Integer	li_control

CHOOSE CASE ai_operacion
	CASE 1//Impresión
		UPDATE dbo.spro_controlventana
		   SET cove_estado = 1
		 WHERE clie_codigo = :ai_cliente
			AND plde_codigo = :al_planta
			AND paen_numero = :al_pallet
			AND cove_nropro = :al_proceso
			AND cove_estado = 0;
			
		If SqlCa.SQLCode = -1 Then
			F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
		Else
			Commit;
		End If
		
	CASE 2//Reimpresión
		UPDATE dbo.spro_controlventana
		   SET cove_reimpr = cove_reimpr + :ai_impresiones
		 WHERE clie_codigo = :ai_cliente
			AND plde_codigo = :al_planta
			AND paen_numero = :al_pallet
			AND cove_nropro = :al_proceso
			AND cove_estado = 1;
			
		If SqlCa.SQLCode = -1 Then
			F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
		Else
			Commit;
		End If
End CHOOSE
		
Return True
End function

public function boolean existe (integer ai_cliente, long al_planta, long al_pallet, integer ai_operacion, long al_proceso, integer ai_cajas);Integer	li_control

CHOOSE CASE ai_operacion
	CASE 1//Impresión
		SELECT clie_codigo
		  INTO :li_control
		  FROM dbo.spro_controlventana
		 WHERE clie_codigo = :ai_cliente
			AND plde_codigo = :al_planta
			AND paen_numero = :al_pallet
			AND cove_nropro = :al_proceso
			AND cove_estado = 0;
			
		If SqlCa.SQLCode = -1 Then
			F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
			Return False
		
		ElseIf SqlCa.SQLCode = 100 Then
			SELECT clie_codigo
			  INTO :li_control
			  FROM dbo.spro_controlventana
			 WHERE clie_codigo = :ai_cliente
				AND plde_codigo = :al_planta
				AND paen_numero = :al_pallet
				AND cove_nropro = :al_proceso
				AND cove_estado = 1;
				
			If SqlCa.SQLCode = -1 Then
				F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
				Return False
				
			ElseIf SqlCa.SQLCode = 100 Then
				SELECT clie_codigo
				  INTO :li_control
				  FROM dbo.spro_controlventana
				 WHERE clie_codigo = :ai_cliente
					AND plde_codigo = :al_planta
					AND paen_numero = :al_pallet
					AND cove_nropro = :al_proceso
					AND cove_estado = 3;
					
				If SqlCa.SQLCode = -1 Then
					F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
					Return False
					
				ElseIf SqlCa.SQLCode = 100 Then
					InsertaControl(ai_cliente, al_planta, al_pallet, ai_cajas, al_proceso)
					
				Else
					MessageBox("Validación Tarjas", "El Número de Tarja ha sido marcado como No Vigente en tabla de control de impresiones")
					Return False
					
				End If
			Else
				MessageBox("Validación Tarjas", "La Ventana solicitada ya fue impresa, debe ReImprimir")
				Return False
				
			End If
		End If
		
	CASE 2//Reimpresión
		SELECT clie_codigo
		  INTO :li_control
		  FROM dbo.spro_controlventana
		 WHERE clie_codigo = :ai_cliente
			AND plde_codigo = :al_planta
			AND paen_numero = :al_pallet
			AND cove_nropro = :al_proceso
			AND cove_estado = 1;
			
		If SqlCa.SQLCode = -1 Then
			F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
			Return False
		
		ElseIf IsNull(li_control) OR SqlCa.SQLCode = 100 Then
			SELECT clie_codigo
			  INTO :li_control
			  FROM dbo.spro_controlventana
			 WHERE clie_codigo = :ai_cliente
				AND plde_codigo = :al_planta
				AND paen_numero = :al_pallet
				AND cove_nropro = :al_proceso
				AND cove_estado = 0;
				
			If SqlCa.SQLCode = -1 Then
				F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
				Return False
				
			ElseIf IsNull(li_control) OR SqlCa.SQLCode = 100 Then
				MessageBox("Validación Tarjas", "El Número de Tarja no existe en tabla de control de impresiones,"+&
						 "~r~no ha sido marcado como No Vigente")
				Return False
				
			Else
				MessageBox("Validación Tarjas", "La Ventana solicitada no ha sido impresa, debe Imprimir")
				Return False
				
			End If
		End If
		
End CHOOSE
		
Return True
End function

public function boolean validarango (integer ai_cliente, long al_planta, long al_pallet, integer ai_operacion, integer ai_impresiones, long al_proceso, integer ai_cajas);Integer	li_control, li_tope, li_hechas
Boolean	lb_Retorno = True

Choose Case ai_operacion
	Case 1
		SELECT Count(*)
		  INTO :li_control
		  FROM dbo.spro_correlfolio
		 WHERE clie_codigo = :ai_cliente
			AND plde_codigo = :al_planta
			AND :al_pallet BETWEEN spco_corini AND spco_corter
			AND spco_estado = 0;
			
		If SqlCa.SQLCode = -1 Then
			F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_correlfolio")		
			lb_Retorno =  False
		ElseIf li_control = 0 Then
			MessageBox("Validación Tarjas", "El Número de Tarja utilizado para el Pallet no corresponde al Rango Valido")
			lb_Retorno =  False
		End If
		
		Return Existe(ai_cliente, al_planta, al_pallet, ai_operacion, al_proceso, ai_cajas)
	Case 2
		If Existe(ai_cliente, al_planta, al_pallet, ai_operacion, al_proceso, ai_cajas) Then
			SELECT cove_topere, cove_reimpr
			  INTO :li_tope, :li_hechas
			  FROM dbo.spro_controlventana
			 WHERE clie_codigo = :ai_cliente
				AND plde_codigo = :al_planta
				AND paen_numero = :al_pallet
				AND cove_nropro = :al_proceso
				AND cove_estado = 1;
				
//			If	valida_inspeccion(al_pallet,ai_cliente,al_planta)  Then
				li_tope = 5
//			End If	
			
			If SqlCa.SQLCode = -1 Then
				F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
				lb_Retorno =  False
			Else
				If li_tope < li_hechas + ai_impresiones Then
					MessageBox("Validación Tarjas", "La cantidad de reimpresiones solicitadas sobrepasa el tope~r~n" + &
															  "Impresiones realizadas 	: " + String(li_hechas) + "~r~n" + &
															  "Tope Impresiones		: " + String(li_tope) + "~r~n" + &
															  "Impresiones restantes 	: " + String(li_tope - li_hechas) + "~r~n" + &
															  "Impresiones Solicitadas	: " + String(ai_impresiones))
					lb_Retorno =  False
				End If
			End If
		Else
			lb_Retorno =  False
		End If
End Choose

Return lb_Retorno
end function

public subroutine insertacontrol (integer ai_cliente, long al_planta, long al_pallet, integer ai_cajas, long al_proceso);INSERT INTO dbo.spro_controlventana (clie_codigo, plde_codigo, paen_numero, cove_nropro, 
												cove_estado, cove_topere, cove_reimpr, cove_cancaj, sist_codigo) 
SELECT :ai_cliente, :al_planta, :al_pallet, :al_proceso,
		  0, par.empr_careim, 0, :ai_cajas, :gstr_apl.CodigoSistema
  FROM dbo.parempresa as par;

If SqlCa.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
	Rollback;
Else
	Commit;
End If
end subroutine

public function boolean valida_inspeccion (long al_pallet, integer ai_cliente, integer ai_planta);Long	ll_cont

SELECT count()
  INTO :ll_cont
  FROM dbo.palletencab
 WHERE clie_codigo = :ai_cliente
	AND plde_codigo = :ai_planta
	AND paen_numero = :al_pallet
	AND paen_inspec = 1
	AND dest_codigo in (336,330);
			
If SqlCa.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
	Return False
ElseIf ll_cont > 0 Then
	Return True
End If	

Return False
End function

public function boolean actualizacorrelativo (integer ai_cliente, long al_planta, long al_pallet);Boolean lb_Retorno 

UPDATE dbo.spro_correlfolio
	SET spco_ultcor	= :al_Pallet
 WHERE clie_codigo = :ai_cliente
	AND plde_codigo = :al_planta
	AND spco_estado = 0;
	
If SqlCa.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_correlfolio")
	lb_Retorno = False
Else
	Commit;
	lb_Retorno = True
End If
				
Return lb_Retorno 
end function

public function long correlativo (integer ai_cliente, long al_planta);Long	ll_Correlativo

SELECT IsNull(spco_ultcor, 0) + 1
  INTO :ll_Correlativo
  FROM dbo.spro_correlfolio
 WHERE clie_codigo = :ai_cliente
	AND plde_codigo = :al_planta
	AND spco_estado = 0;
	
If SqlCa.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_correlfolio")		
	Return 0
End If

Return ll_Correlativo
end function

public function long correlativo (integer ai_cliente, long al_planta, integer ai_tipo);Long	ll_Correlativo

SELECT IsNull(spco_ultcor, 0) + 1
  INTO :ll_Correlativo
  FROM dbo.spro_correlfolio
 WHERE clie_codigo = :ai_cliente
	AND plde_codigo = :al_planta
	And paen_tipopa = :ai_Tipo
	AND spco_estado = 0;
	
If SqlCa.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_correlfolio")		
	ll_Correlativo = 0
Else
	If Not ValidaRango(ai_cliente, al_planta, ll_Correlativo, ai_Tipo, True, Sqlca) Then
		ll_Correlativo = -1
	End if
End If

Return ll_Correlativo
end function

public function boolean validarango (integer ai_cliente, long al_planta, long al_pallet, integer ai_tipo, boolean mensaje, transaction transaccion);Integer	li_control
Boolean	lb_Retorno = True

SELECT Count(*)
  INTO :li_control
  FROM dbo.spro_correlfolio
 WHERE clie_codigo = :ai_cliente
	AND plde_codigo = :al_planta
	And paen_tipopa = :ai_Tipo
	AND :al_pallet BETWEEN spco_corini AND spco_corter
	AND spco_estado = 0
Using Transaccion;
	
If Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Correlativo Folios")		
	lb_Retorno =  False
ElseIf li_control = 0 Then
	If Mensaje Then MessageBox("Validación Tarjas", "El Número de Folio utilizado para el Pallet no corresponde al Rango Valido")
	lb_Retorno =  False
End If

Return lb_Retorno
end function

public function boolean actualizacorrelativo (integer ai_cliente, long al_planta, long al_pallet, integer ai_tipo);Boolean lb_Retorno 


UPDATE dbo.spro_correlfolio
	SET spco_ultcor	= :al_Pallet
 WHERE clie_codigo = :ai_cliente
	AND plde_codigo = :al_planta
	and paen_tipopa = :ai_Tipo
	AND spco_estado = 0;
	
If SqlCa.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_correlfolio")
	lb_Retorno  = False
Else
	Commit;
	lb_Retorno  = True
End If
				
Return lb_Retorno 
end function

public function boolean of_disponible (integer ai_cliente, long al_planta, integer ai_tipo, transaction transaccion);Long		Fin, Ultima, Disponibles, Tope = 50
Boolean	lb_Retorno = True

SELECT spco_corter, spco_ultcor
	INTO :Fin, :Ultima
  FROM dbo.spro_correlfolio
 WHERE clie_codigo = :ai_Cliente
	AND plde_codigo = :al_Planta
	And paen_tipopa = :ai_Tipo
	AND spco_estado = 0
Using Transaccion;
	
If Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Correlativo Folios")		
	lb_Retorno =  False
Else
	Disponibles = Fin - Ultima
	
	If Tope >= Disponibles Then
		MessageBox('Folios Disponibles', 'Quedan disponibles: ' + String(Disponibles) + '~r~nTener en cuenta solicitar nuevo correlativo.', Information!)
		
	End If	
End If

Return lb_Retorno
end function

public function boolean of_existe (integer cliente, long planta, integer tipo, boolean mensaje, transaction transaccion);Integer	li_control
Boolean	lb_Retorno = True

SELECT Count(clie_codigo)
  INTO :li_control
  FROM dbo.spro_correlfolio
 WHERE clie_codigo = :Cliente
	AND plde_codigo = :Planta
	AND paen_tipopa = :Tipo
	And spco_estado = 0
Using transaccion;
	
If transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_controlventana")
	lb_Retorno =  False
ElseIf li_Control = 0 Then
	If Mensaje Then MessageBox('Atencion', 'No existen correlativos vigente.')
	lb_Retorno = False
End If
		
Return lb_Retorno
end function

public function boolean of_actualiza (long cliente, long planta, integer tipo, ref datawindow adw);Boolean 		lb_Retorno = True
String 		ls_URL, ls_Busca, ls_Tipo = 'PUCHO'
Long			ll_Busca

RESTClient				lnv_RestClient
uo_plantadesp			luo_Planta
DataStore				lds_Folios

If Tipo = 1 Then ls_Tipo = 'PALLET'

If Not of_Existe(Cliente, Planta, Tipo, False, Sqlca) Then	
	luo_Planta	=	Create uo_plantadesp
	lds_Folios	=	Create DataStore
	
	lnv_RestClient	=	Create RESTClient
	lnv_RestClient.SetRequestHeader ("Content-Type", "application/json;charset=UTF-8")	
	
	luo_Planta.Existe(Planta, False, SQLCA)
	
	lds_Folios.DataObject = 'dw_mues_correlfolio'
	ls_URL = _URL + 'api/Correlfolio/Retrieve/' + String(Cliente)
	lnv_RestClient.Retrieve(lds_Folios, ls_URL)

	If lds_Folios.RowCount() > 0 Then			
		ls_Busca = 'plde_codigo = '  + String(Planta) + ' and paen_tipopa = ' + String(Tipo) + ' and spco_estado = 0' 
		ll_Busca = lds_Folios.Find(ls_Busca, 1, lds_Folios.RowCount())
	
		If ll_Busca > 0 Then
			If Not of_ValidaRango(Cliente, Planta, Tipo, lds_Folios.Object.spco_corini[ll_Busca], True, Sqlca) Then
				lds_Folios.RowsCopy(ll_Busca, ll_Busca, Primary!, adw, 1, Primary!)
			End If
		End If
	Else
		MessageBox('Atencion', 'No Existen Folios vigentes para tipo :' + ls_Tipo + ', Packing: ' + luo_Planta.Nombre)	
	End If
	
	Destroy lds_Folios
	Destroy luo_Planta
	Destroy lnv_RestClient
Else
	MessageBox('Atencion', 'No Existen Folios en estado cerrado, para tipo :' + ls_Tipo)
End If

Return lb_Retorno
end function

public function boolean of_validarango (integer cliente, long planta, integer tipo, long correlativo, boolean mensaje, transaction transaccion);Integer	li_control
Boolean	lb_Retorno = True

SELECT Count(*)
	Into :li_Control
  FROM dbo.spro_correlfolio
 WHERE clie_codigo = :Cliente
	AND plde_codigo = :Planta
	AND paen_tipopa = :Tipo
	And :Correlativo between spco_corini and spco_corter
Using transaccion;
	
If Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Correlativo Folios")		
	lb_Retorno =  False
ElseIf li_control = 0 Then
//	If Mensaje Then MessageBox("Validación Correlativo", "El Número de Folio utilizado para el Pallet no corresponde al Rango Valido")
	lb_Retorno =  False
ElseIf li_control > 0 Then
	If Mensaje Then MessageBox("Validación Correlativo", "El Número de Folio esta contenido en rangos anteriores")
	lb_Retorno =  True
End If

Return lb_Retorno
end function

on uo_controlventanas.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_controlventanas.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

