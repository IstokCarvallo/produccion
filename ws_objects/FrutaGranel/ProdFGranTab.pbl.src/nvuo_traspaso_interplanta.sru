$PBExportHeader$nvuo_traspaso_interplanta.sru
forward
global type nvuo_traspaso_interplanta from nonvisualobject
end type
end forward

global type nvuo_traspaso_interplanta from nonvisualobject descriptor "PS_JaguarProject" = "" 
event ue_traspasadatos ( )
event ue_recuperadatos ( )
event ue_guardar ( )
event ue_antesguardar ( )
event ue_borrar ( )
end type
global nvuo_traspaso_interplanta nvuo_traspaso_interplanta

type variables
Transaction							SQLTRA
Boolean								ib_Conectado, lb_LocalRemoto

Integer								ii_Planta, ii_Tipo, ii_Movto, ii_Cliente, ii_sentido,&
										ii_ManAut, retorno, ii_plantades, ii_plantaorigen
										
Long									il_guiasii

Window								w_parent			

uo_categorias						iuo_categoria
uo_periodofrio						iuo_frio
uo_condicioncc						iuo_condicion
uo_variedades						iuo_variedad
uo_envases							iuo_envases
uo_camiones						iuo_camion
uo_especie							iuo_especie
uo_plantadesp						iuo_planta
uo_productores						iuo_productor
uo_Clientesprod					iuo_cliente
uo_motivodespacho				iuo_motivo
uo_transportista					iuo_transp
uo_bins								iuo_bins
uo_camarasfrigo					iuo_camarasbode
uo_spro_movtofrutagranenca	iuo_movto

uo_lotesfrutagranel				iuo_lotes

DataStore							dw_movtobinstrans, dw_movtobins

DataStore							dw_archivos,dw_envaencatrans,dw_loteencatrans,&
										dw_movtoencatrans, dw_movtodetatrans,dw_lotedetatrans,&
										dw_envadetatrans,dw_envadeta,dw_lotedeta,dw_movtodeta,&
										dw_movtoenca,dw_loteenca,dw_envaenca,&
										dw_binstrans,dw_bins,dw_granpesatrans,&
										dw_granpesa
end variables

forward prototypes
public function boolean wf_actualiza_db (boolean borrando)
public function boolean validalotes ()
public function boolean validamovto ()
public function boolean validaenvases ()
public subroutine enviamail ()
public function boolean validabins ()
public function boolean eliminatrans ()
private function boolean of_cargadatos ()
private function boolean of_conectatrans (integer ai_planta)
private function boolean of_generaarchivos ()
private function boolean of_cargaarchivosplanos ()
private function boolean of_cargadatos_trans ()
end prototypes

event ue_traspasadatos();Str_Busqueda	lstr_busq
Integer			li_resp

dw_movtoEnca.SetTransObject(sqlca)
dw_movtoDeta.SetTransObject(sqlca)
dw_loteEnca.SetTransObject(sqlca)
dw_loteDeta.SetTransObject(sqlca)
dw_envaEnca.SetTransObject(sqlca)
dw_envaDeta.SetTransObject(sqlca)
dw_granpesa.SetTransObject(sqlca)
dw_movtobins.SetTransObject(sqlca)
dw_bins.SetTransObject(sqlca)

dw_movtoEncaTrans.Reset()
dw_movtoDetaTrans.Reset()
dw_loteEncaTrans.Reset()
dw_loteDetaTrans.Reset()
dw_envaEncaTrans.Reset()
dw_envaDetaTrans.Reset()
dw_granpesaTrans.Reset()
dw_movtoBinsTrans.Reset()
dw_binsTrans.Reset()

Message.DoubleParm = 0
CHOOSE Case ii_sentido
	Case 1
		This.TriggerEvent("ue_recuperadatos")
		
		If Message.DoubleParm <> -1 Then
			If of_Cargadatos() Then
				This.Triggerevent("ue_guardar")
			Else
				MessageBox("Error", "La carga de datos presento problemas")
			End If
		Else
			MessageBox("Error", "Imposible regenerar datos de Despacho")
		End If
	Case 2
		If ii_ManAut = 2 Then
	
			lstr_Busq.Argum[01]	=	String(ii_planta)
			lstr_Busq.Argum[02]	=	String(ii_tipo)
			lstr_Busq.Argum[03]	=	"3"
			lstr_Busq.Argum[04]	=	String(Today(), 'dd/mm/yyyy')
			lstr_Busq.Argum[10]	=	String(ii_cliente)
			
			OpenWithParm(w_busc_spro_movtofrutagranenca_trans, lstr_busq)
			
			lstr_busq	=	Message.PowerObjectParm
			
			If lstr_Busq.Argum[8] <> "" Then
				ii_plantaorigen	=	Integer(lstr_Busq.Argum[01])
				ii_tipo				=	Integer(lstr_Busq.Argum[02])
				ii_movto			=	Integer(lstr_Busq.Argum[03])
				ii_cliente			=	Integer(lstr_Busq.Argum[10])
				This.TriggerEvent("ue_recuperadatos")
			Else
				Return				//LRBB 16.ene.2014	no muestra mensajes de errores cuando se cancela la importacion de datos desde transferencia
			End If
		Else
			If Not of_CargaArchivosPlanos() Then
				MessageBox("Error", "La carga de Archivos Planos presento problemas")
				Message.DoubleParm = -1				
			End If
		End If
		
		If Message.DoubleParm <> -1 Then
			If of_Cargadatos_Trans() Then
				w_parent.TriggerEvent("ue_carga_detalle")				
			Else
				MessageBox("Error", "La carga de datos presento problemas")				
			End If
		Else
			MessageBox("Error", "Imposible regenerar datos para Recepción")			
		End If		
	Case 3
		If of_conectatrans(ii_plantades) Then
			If Eliminatrans() Then
				MessageBox("Operacion Exitosa", "Los datos han sido eliminados de las tablas transitorias,"+&
							  "~r~nse procedera con la eliminación local")							  
			Else
				li_resp = MessageBox("Error de Conexión", "No se ha podido eliminar los datos de las tablas transitorias,"+&
											"~r~n ¿Desea eliminar la recepción de todas formas?", Exclamation!, YesNo!)
				If li_resp = 2 Then Message.DoubleParm = -1
			End If
		Else
			li_resp = MessageBox("Error de Conexión", "No se ha podido establecer comunicación con la base de datos de destino,"+&
										"~r~n ¿Desea eliminar la recepción de todas formas?", Exclamation!, YesNo!)		
			If li_resp = 2 Then Message.DoubleParm = -1
		End If
End CHOOSE
end event

event ue_recuperadatos();Integer	li_filas, respuesta
Boolean 	lb_retorno = True, lb_estado = True

If ii_sentido = 1 Then
	DO
		If dw_MovtoEnca.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtofrutagranenca", StopSign!)
		End If
		
		If dw_MovtoDeta.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1	AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtofrutagrandeta", StopSign!)
		End If
		
		If dw_LoteEnca.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla lotesfrutagranel", StopSign!)
		End If

		If dw_LoteDeta.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla lotesfrutagrandeta", StopSign!)
		End If
		
		If dw_EnvaEnca.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtoenvaenca", StopSign!)
		End If
		
		If dw_EnvaDeta.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtoenvadeta", StopSign!)
		End If
		
		If dw_granPesa.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtofrutagranpesa", StopSign!)
		End If
		If dw_movtobins.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtobins", StopSign!)
		End If
		
		If dw_bins.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla bins", StopSign!)
		End If
		
	LOOP WHILE respuesta = 1
Else
	If NOT ib_Conectado Then
		dw_movtoEncaTrans.SetTransObject(sqlca)
		dw_movtoDetaTrans.SetTransObject(sqlca)
		dw_loteEncaTrans.SetTransObject(sqlca)
		dw_loteDetaTrans.SetTransObject(sqlca)
		dw_envaEncaTrans.SetTransObject(sqlca)
		dw_envaDetaTrans.SetTransObject(sqlca)
		dw_granpesaTrans.SetTransObject(sqlca)
		dw_movtoBinsTrans.SetTransObject(sqlca)
		dw_binsTrans.SetTransObject(sqlca)
	End If
	
	DO
		If dw_MovtoEncaTrans.Retrieve(ii_plantaorigen, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.",  Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtofrutagranenca_trans", StopSign!)
		End If
		
		If dw_MovtoDetaTrans.Retrieve(ii_plantaorigen, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos",  "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtofrutagrandeta_trans", StopSign!)
		End If
		
		If dw_LoteEncaTrans.Retrieve(ii_plantaorigen, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos",  "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla lotefrutagranenca_trans", StopSign!)
		End If
		
		If dw_LoteDetaTrans.Retrieve(ii_plantaorigen, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla lotefrutagrandeta_trans", StopSign!)
		End If
		
		If dw_EnvaEncaTrans.Retrieve(ii_plantaorigen, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla envaenca_trans", StopSign!)
		End If
		
		If dw_EnvaDetaTrans.Retrieve(ii_plantaorigen, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla envadeta_trans", StopSign!)
		End If
		
		If dw_granPesaTrans.Retrieve(ii_plantaorigen, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtofrutagranpesa_trans", StopSign!)
		End If
		
		If dw_movtoBinsTrans.Retrieve(ii_plantaorigen, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Movtobins_trans", StopSign!)
		End If
		
		If dw_binsTrans.Retrieve(ii_plantaorigen, ii_tipo, ii_movto, ii_cliente) = -1 AND lb_estado Then
			respuesta 	= 	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Message.DoubleParm = -1
			lb_estado = False
			Messagebox("Error en la recuperación de datos", "Error en tabla Bins_trans", StopSign!)
		End If
		
		If lb_estado Then
			If iuo_movto.existe(ii_planta, ii_cliente, ii_tipo, ii_movto, True, sqlca) Then Message.DoubleParm = -1
		End If
		
	LOOP WHILE respuesta = 1
End If
end event

event ue_guardar();SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

If of_Conectatrans(ii_plantades) Then 
	w_main.SetMicroHelp("Comenzando Proceso Automatico.")	
	If wf_actualiza_db(False) Then
		MessageBox('Atencion...', 'Información transferida con exito.', Information!, Ok!)
		w_main.SetMicroHelp("Información Grabada.")	
	Else
		MessageBox('Atencion...', 'No se puede Grabar información.', StopSign!, Ok!)
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN
	End If
Else
	w_main.SetMicroHelp("Comenzando Proceso Via E-Mail")	
	If of_GeneraArchivos() Then
		EnviaMail()
	End If
End If
end event

event ue_borrar();Integer	li_status
SetPointer(HourGlass!)

w_main.SetMicroHelp("Eliminando información...")

Message.DoubleParm = 0

If lb_LocalRemoto Then
	If of_Conectatrans(ii_plantades) Then
		w_main.SetMicroHelp("Comenzando Proceso de Eliminación Automatica.")
		If wf_actualiza_db(True) Then
			w_main.SetMicroHelp("Información Eliminada.")
		Else
			w_main.SetMicroHelp("No se puede Eliminar información.")
			Message.DoubleParm = -1
			RETURN
		End If
	Else
		w_main.SetMicroHelp("No se ha podido conectar a base de datos Destino")
	End If
Else
	w_main.SetMicroHelp("Comenzando Proceso de Eliminación Automatica.")
	If wf_actualiza_db(True) Then
		w_main.SetMicroHelp("Información Eliminada.")
	Else
		w_main.SetMicroHelp("No se puede Eliminar información.")
		Message.DoubleParm = -1
		RETURN
	End If
End If
end event

public function boolean wf_actualiza_db (boolean borrando);Boolean 		lb_retorno
Transaction	lt_trans

lt_trans		=	Create Transaction

If NOT IsValid(SQLTra) Then
	lt_trans	=	SQLCa
Else
	lt_trans	=	SQLTra
End If
	
If borrando Then
	If dw_movtoencatrans.RowCount() > 0 Then
		DO
			If dw_movtoencatrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_movtoencatrans.RowCount() > 0
	End If

	If dw_movtodetatrans.RowCount() > 0 Then
		DO
			If dw_movtodetatrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_movtodetatrans.RowCount() > 0
	End If
				
	If dw_movtobinstrans.RowCount() > 0 Then
		DO
			If dw_movtobinstrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_movtobinstrans.RowCount() > 0
	End If

	If dw_envaencatrans.RowCount() > 0 Then
		DO
			If dw_envaencatrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_envaencatrans.RowCount() > 0
	End If
	 
	If dw_envadetatrans.RowCount() > 0 Then
		DO
			If dw_envadetatrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_envadetatrans.RowCount() > 0
	End If

	If dw_loteencatrans.RowCount() > 0 Then
		DO
			If dw_loteencatrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_loteencatrans.RowCount() > 0
	End If
	
	If dw_lotedetatrans.RowCount() > 0 Then
		DO
			If dw_lotedetatrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_lotedetatrans.RowCount() > 0
	End If
	
	If dw_binstrans.RowCount() > 0 Then
		DO
			If dw_binstrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_binstrans.RowCount() > 0
	End If
	
		If dw_granpesaTrans.RowCount() > 0 Then
		Do
			If dw_granpesaTrans.DeleteRow(1) = -1 Then Return False
		LOOP WHILE dw_granpesaTrans.RowCount() > 0
	End If
End If


If dw_movtoEncaTrans.Update(True, False) > -1 Then		 					//Encabezado
	If dw_loteEncaTrans.Update(True, False) > -1 Then						//Lotes
		If dw_loteDetaTrans.Update(True, False) > -1 Then					//Detalle de Lotes
			If dw_movtoDetaTrans.Update(True, False) > -1 Then				//Detalle
				If dw_envaEncaTrans.Update(True,False) > -1 Then			//Envases Encabezado
					If dw_envaDetaTrans.Update(True,False) > -1 Then		//Envases Recepcionados
						If dw_granpesaTrans.Update(True,False) > -1 Then	//Detalle Pesaje
							If dw_movtoBinsTrans.Update(True,False) > -1 Then//Movimiento Bins
//								If dw_binsTrans.Update(True,False) > -1 Then //Movimiento Bins
									Commit;
									
									If lt_Trans.SQLCode <> 0 Then
										MessageBox("Error Actualización tablas", "Error Ejecutando Commit General")
									Else
										lb_Retorno	=	True
										
										dw_movtoEncaTrans.ResetUpdate()
										dw_movtoDetaTrans.ResetUpdate()
										dw_loteEncaTrans.ResetUpdate()
										dw_loteDetaTrans.ResetUpdate()
										dw_envaEncaTrans.ResetUpdate()
										dw_envaDetaTrans.ResetUpdate()
										dw_granpesaTrans.ResetUpdate()
										dw_movtoBinsTrans.ResetUpdate()
									End If
//								Else
//									MessageBox("Error Actualización tablas", "Error Actualizando Movimiento Bins")//Movimiento Bins
//									RollBack;
//								End If
							Else
								MessageBox("Error Actualización tablas", "Error Actualizando Movimiento Bins")//Movimiento Bins
//								dw_parent.dw_error.SetTransObject(lt_trans)
//								dw_parent.dw_error.DataObject	=	dw_movtoBinsTrans.DataObject
//								dw_movtoBinsTrans.RowsCopy(1, dw_movtoBinsTrans.RowCount(), Primary!, dw_parent.dw_error, 1, Primary!)
//								dw_parent.dw_error.Update(True,False)
//								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							End If
						Else
							MessageBox("Error Actualización tablas", "Error Actualizando Detalle Pesaje")//Detalle Pesaje
							RollBack;
						End If
					Else
						MessageBox("Error Actualización tablas", "Error Actualizando Envases Recepcionados")//Envases Recepcionados
						RollBack;
					End If
				Else
					MessageBox("Error Actualización tablas", "Error Actualizando Envases Encabezado")//Envases Encabezado
					RollBack;
				End If
			Else
				MessageBox("Error Actualización tablas", "Error Actualizando Detalle Movto")//Detalle
				RollBack;
			End If
		Else
			MessageBox("Error Actualización tablas", "Error Actualizando Detalle de Lotes")//Detalle de Lotes
			RollBack;
		End If
	Else
		MessageBox("Error Actualización tablas", "Error Actualizando Encabezado Lotes")//Lotes
		RollBack;
	End If
Else
	MessageBox("Error Actualización tablas", "Error Actualizando Encabezado Movimiento")//Encabezado Movimiento
	RollBack;
End If

Return lb_retorno
End function

public function boolean validalotes ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_loteencatrans.RowCount()
	
	li_find	=	dw_movtodetatrans.Find("lote_pltcod = " + String(dw_loteencatrans.Object.lote_pltcod[li_fila]) + " AND " +&
												  "lote_espcod = " + String(dw_loteencatrans.Object.lote_espcod[li_fila]) + " AND " +&
												  "lote_codigo = " + String(dw_loteencatrans.Object.lote_codigo[li_fila]) , &
												  1, dw_movtodetatrans.RowCount())
	If li_find < 1 Then
		MessageBox("Error", 	"El Lote " + String(dw_loteencatrans.Object.lote_pltcod[li_fila], '0000-')  + &
												    String(dw_loteencatrans.Object.lote_espcod[li_fila], '00-')   + &
												    String(dw_loteencatrans.Object.lote_codigo[li_fila], '00000 ') + &
									"no pertenece a este Movimiento, no se podra generar recepción")
		lb_retorno = False
	ElseIf NOT iuo_categoria.Existe(dw_loteencatrans.Object.cate_codigo[li_fila], True, sqlca) Then
		lb_retorno = False
	ElseIf NOT iuo_frio.ofp_recupera_periodofrio(sqlca, dw_loteencatrans.Object.pefr_codigo[li_fila], True) Then
		lb_retorno = False
	ElseIf NOT iuo_condicion.Existe(dw_loteencatrans.Object.cocc_codigo[li_fila], True, sqlca) Then
		lb_retorno = False
	ElseIf NOT iuo_especie.Existe(dw_loteencatrans.Object.lote_espcod[li_fila], True, sqlca) Then
		lb_retorno = False
	ElseIf NOT iuo_variedad.Existe(dw_loteencatrans.Object.lote_espcod[li_fila], &
									dw_loteencatrans.Object.vari_codigo[li_fila], True, sqlca) Then
		lb_retorno = False
	ElseIf iuo_lotes.Existe(dw_loteEnca.Object.lote_pltcod[li_fila], &
									dw_loteEnca.Object.lote_espcod[li_fila], &
									dw_loteEnca.Object.lote_codigo[li_fila], False, SQLCA) Then
		dw_loteEnca.SetItemStatus(li_fila, 0, Primary!, NotModIfied!)
	End If
	
	If NOT lb_retorno Then EXIT
NEXT

FOR li_fila = 1 TO dw_lotedetatrans.RowCount()
	
	li_find	=	dw_loteencatrans.Find("lote_pltcod = " + String(dw_lotedetatrans.Object.lote_pltcod[li_fila]) + " AND " +&
												 "lote_espcod = " + String(dw_lotedetatrans.Object.lote_espcod[li_fila]) + " AND " +&
												 "lote_codigo = " + String(dw_lotedetatrans.Object.lote_codigo[li_fila]) , &
												 1, dw_loteencatrans.RowCount())
	If li_find < 1 Then
		MessageBox("Error", "El Lote " + String(dw_lotedetatrans.Object.lote_pltcod[li_fila], '0000-')  + &
												   String(dw_lotedetatrans.Object.lote_espcod[li_fila], '00-')   + &
												   String(dw_lotedetatrans.Object.lote_codigo[li_fila], '00000 ') + &
								  "no posee encabezado disponible, no se podra generar recepción")
		lb_retorno = False
		
	ElseIf NOT iuo_envases.Existe(dw_lotedetatrans.Object.enva_tipoen[li_fila], &
									      dw_lotedetatrans.Object.enva_codigo[li_fila], True, Sqlca) Then
		lb_retorno = False
		
	ElseIf iuo_lotes.Existe(dw_loteDeta.Object.lote_pltcod[li_fila], &
									dw_loteDeta.Object.lote_espcod[li_fila], &
									dw_loteDeta.Object.lote_codigo[li_fila], False, SQLCA) Then
		dw_loteDeta.SetItemStatus(li_fila, 0, Primary!, NotModIfied!)
		
	End If
	
	If NOT lb_retorno Then EXIT
NEXT

If lb_retorno Then lb_retorno = ValidaMovto()
 
Return lb_retorno 
End function

public function boolean validamovto ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_movtoencatrans.RowCount()
	
	If NOT iuo_camion.Existe(dw_movtoencatrans.Object.cami_clasIfi[li_fila],&
									 dw_movtoencatrans.Object.cami_patent[li_fila], true, sqlca) Then
		lb_retorno = False
//	ElseIf NOT iuo_especie.Existe(dw_movtoencatrans.Object.espe_codigo[li_fila], true, sqlca) Then
//		lb_retorno = False
	ElseIf NOT iuo_planta.Existe(dw_movtoencatrans.Object.plde_codigo[li_fila], true, sqlca) Then
		lb_retorno = False
	ElseIf NOT iuo_productor.Existe(dw_movtoencatrans.Object.prod_codigo[li_fila], true, sqlca) Then
		lb_retorno = False
	ElseIf NOT iuo_cliente.Existe(dw_movtoencatrans.Object.clie_codigo[li_fila], true, sqlca) Then
		lb_retorno = False
//	ElseIf NOT iuo_motivo.Existe(dw_movtoencatrans.Object.moti_codigo[li_fila], true, sqlca) Then
//		lb_retorno = False	
	End If
	
	If NOT lb_retorno Then EXIT
NEXT

FOR li_fila = 1 TO dw_movtodetatrans.RowCount()
	li_find	=	dw_loteencatrans.Find("lote_pltcod = " + String(dw_movtodetatrans.Object.lote_pltcod[li_fila]) + " AND " +&
												 "lote_espcod = " + String(dw_movtodetatrans.Object.lote_espcod[li_fila]) + " AND " +&
												 "lote_codigo = " + String(dw_movtodetatrans.Object.lote_codigo[li_fila]) , &
												  1, dw_loteencatrans.RowCount())
	If li_find < 1 Then
		MessageBox("Error", "El Lote " + String(dw_movtodetatrans.Object.lote_pltcod[li_fila], '0000-')  + &
												   String(dw_movtodetatrans.Object.lote_espcod[li_fila], '000-')   + &
												   String(dw_movtodetatrans.Object.lote_codigo[li_fila], '00000 ') + &
												   "no pertenece a este Movimiento, no se podra generar recepción")
		lb_retorno = False
	End If
NEXT

If lb_retorno Then lb_retorno = ValidaEnvases()

Return lb_retorno
End function

public function boolean validaenvases ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_envaencatrans.RowCount()
	
	If NOT iuo_camion.Existe(dw_envaencatrans.Object.cami_clasIfi[li_fila],&
									 dw_envaencatrans.Object.cami_patent[li_fila], true, sqlca) Then
		lb_retorno = False
	ElseIf NOT iuo_planta.Existe(dw_envaencatrans.Object.plde_codigo[li_fila], true, sqlca) Then
		lb_retorno = False
	ElseIf NOT iuo_productor.Existe(dw_envaencatrans.Object.prod_codigo[li_fila], true, sqlca) Then
		lb_retorno = False
	ElseIf NOT iuo_cliente.Existe(dw_envaencatrans.Object.clie_codigo[li_fila], true, sqlca) Then
		lb_retorno = False
//	ElseIf NOT iuo_motivo.Existe(dw_envaencatrans.Object.moti_codigo[li_fila], true, sqlca) Then
//		lb_retorno = False
	ElseIf NOT iuo_transp.Existe(dw_envaencatrans.Object.tran_codigo[li_fila], true, sqlca) Then
		lb_retorno = False
	End If
	If NOT lb_retorno Then EXIT
	
NEXT

FOR li_fila = 1 TO dw_envadetatrans.RowCount()
	If NOT iuo_envases.Existe(dw_envadetatrans.Object.enva_tipoen[li_fila], &
									      			dw_envadetatrans.Object.enva_codigo[li_fila], True, Sqlca) Then
		lb_retorno = False
	End If
	If NOT lb_retorno Then EXIT
NEXT

If lb_retorno Then lb_retorno = ValidaBins()

Return lb_retorno
End function

public subroutine enviamail ();String		ls_Nombre, ls_NomReporte, ls_Archivo, ls_DirectorioAct 
Long			ll_Fila, ll_consignatario, ll_Archivo
Boolean		lb_Existe
Integer		li_imprimio
str_parms	lstr_parms

SetPointer(HourGlass!)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_DirectorioAct)

ls_archivo = String(ii_planta, '0000') + String(ii_cliente, '000') + String(ii_Movto, '0000000') 

ls_NomReporte									=	'Archivos Planos Despacho ' + ls_archivo
lstr_parms.string_arg[1]					=	String(1)
lstr_parms.string_arg[2]					=	ls_NomReporte
lstr_parms.string_arg[3]					=	String(10)
lstr_parms.string_arg[30]					=	String(ii_planta)

ll_Archivo = 1

lstr_parms.string_arg[ll_Archivo+3]		=	ls_DirectorioAct + '\' + ls_archivo + "Indice.txt"

FOR ll_Archivo = 1 TO 9
	lstr_parms.string_arg[ll_Archivo+4]	=	ls_DirectorioAct + '\' + dw_archivos.Object.Nombre[ll_archivo]
NEXT

OpenWithParm(w_correo_traspaso_interplanta, lstr_parms)

SetPointer(Arrow!)
End subroutine

public function boolean validabins ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_movtoBinsTrans.RowCount()
	
//	li_find	=	dw_movtodetatrans.Find("lote_pltcod = " + String(ii_plantaorigen) + " AND " +&
//												  "lote_espcod = " + String(dw_movtoBinsTrans.Object.lote_espcod[li_fila]) + " AND " +&
//												  "lote_codigo = " + String(dw_movtoBinsTrans.Object.lote_codigo[li_fila]) , &
//												  1, dw_loteencatrans.RowCount())
//	If li_find < 1 Then
//		MessageBox("Error", 	"El Lote " + String(ii_plantaorigen, '0000-')  + &
//												   String(dw_movtoBinsTrans.Object.lote_espcod[li_fila], '000-')   + &
//												   String(dw_movtoBinsTrans.Object.lote_codigo[li_fila], '00000 ') + &
//									"no pertenece a este Movimiento, no se podra generar recepción")
//		lb_retorno = False
////	ElseIf NOT iuo_bins.Existe(dw_movtoBinsTrans.Object.clie_codigo[li_fila],&
////										dw_movtoBinsTrans.Object.plde_codigo[li_fila],&
////										dw_movtoBinsTrans.Object.bins_numero[li_fila], sqlca, True) Then
////		lb_retorno = False
//	Else
	If NOT iuo_camarasbode.Existe(dw_movtoBinsTrans.Object.plde_codigo[li_fila], &
												 dw_movtoBinsTrans.Object.cama_codigo[li_fila], True,sqlca) Then
		lb_retorno = False
	End If
	
	If NOT lb_retorno Then EXIT
NEXT

Return lb_retorno
End function

public function boolean eliminatrans ();Integer	li_resp

 DECLARE Elimina PROCEDURE FOR dbo.fgran_elimina_interplanta  
         @cliente = :ii_cliente,   
         @planta 	= :ii_plantades,   
         @tipo 	= :ii_tipo,   
         @numero 	= :ii_movto
			USING SQLTRA;

Execute Elimina; 

If SQLTRA.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLTRA, "Eliminación de datos trans en base de destino")
	Return False
	
ElseIf SQLTRA.SQLCode = 0 Then
	Fetch Elimina into :li_resp;
	Close Elimina;
	If li_resp <> 0 Then 
		Return False
	Else
		Return True
	End If
End If
End function

private function boolean of_cargadatos ();Integer	li_fila, li_estado
Boolean 	lb_retorno = True
dw_loteEncaTrans.RowCount()

If dw_movtoEnca.RowsCopy(1, dw_movtoEnca.RowCount(), Primary!,dw_MovtoEncaTrans, 1, Primary!) = -1 OR &
	dw_movtoDeta.RowsCopy(1, dw_movtoDeta.RowCount(), Primary!,dw_movtoDetaTrans, 1, Primary!) = -1 OR &
	dw_loteEnca.RowsCopy(1, dw_loteEnca.RowCount(), Primary!, dw_loteEncaTrans, 1, Primary!) = -1 OR &
	dw_loteDeta.RowsCopy(1, dw_loteDeta.RowCount(), Primary!, dw_loteDetaTrans, 1, Primary!) = -1 OR &
	dw_envaEnca.RowsCopy(1, dw_envaEnca.RowCount(), Primary!, dw_envaEncaTrans, 1, Primary!) = -1 OR &
	dw_envaDeta.RowsCopy(1, dw_envaDeta.RowCount(),  Primary!, dw_envaDetaTrans, 1, Primary!) = -1 OR &
	dw_movtobins.RowsCopy(1, dw_movtobins.RowCount(), Primary!,dw_movtobinsTrans, 1, Primary!) = -1 OR &
	dw_granpesa.RowsCopy(1, dw_granpesa.RowCount(),  Primary!, dw_granpesaTrans, 1, Primary!) = -1 OR &
	dw_bins.RowsCopy(1, dw_bins.RowCount(), Primary!, dw_binsTrans, 1, Primary!) = -1 Then
	lb_retorno = False
Else
	dw_loteEncaTrans.RowCount()
	FOR li_fila = 1 TO dw_movtoEncaTrans.RowCount()
		dw_movtoEncaTrans.Object.mfge_guisii[li_fila]	=	il_guiasii
		
		li_estado = dw_movtoEncaTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_movtoDetaTrans.RowCount()
	//	dw_loteEncaTrans.Object.cama_codigo[li_fila] 	= 	0
		
		li_estado = dw_movtoDetaTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_loteEncaTrans.RowCount()
		dw_loteEncaTrans.Object.lote_guisii[li_fila]	=	il_guiasii

		li_estado = dw_loteEncaTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_loteDetaTrans.RowCount()
		li_estado = dw_loteDetaTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_envaEncaTrans.RowCount()
		dw_envaEncaTrans.Object.meen_guisii[li_fila]	=	il_guiasii
		
		li_estado = dw_envaEncaTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_envaDetaTrans.RowCount()
		li_estado = dw_envaDetaTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_movtobinsTrans.RowCount()
		//Asigno numero de despacho, para recuperar datos en destino.
		dw_movtobinsTrans.Object.mfge_numero[li_fila] 	= 	ii_movto
		
		//Anulo camara, calle, posición y piso para evitar problemas en destino.
		dw_movtobinsTrans.Object.cama_codigo[li_fila]	= 	0
		dw_movtobinsTrans.Object.fgmb_calle[li_fila] 	= 	0
		dw_movtobinsTrans.Object.fgmb_base[li_fila] 		= 	0
		dw_movtobinsTrans.Object.fgmb_posici[li_fila]	= 	0
		
		//Cambio estado a activo.
		dw_movtobinsTrans.Object.fgmb_estado[li_fila]	= 	1
		
		li_estado = dw_movtobinsTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_granpesaTrans.RowCount()
		//Asigno numero de despacho y tipo mvto, para recuperar datos en destino.
		dw_granpesaTrans.Object.mfge_numero[li_fila] 	=	ii_movto
		dw_granpesaTrans.Object.tpmv_codigo[li_fila] 	= 	ii_tipo
		
		li_estado = dw_granpesaTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_binsTrans.RowCount()
		li_estado = dw_binsTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
End If

Return lb_retorno
end function

private function boolean of_conectatrans (integer ai_planta);String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

If ii_sentido = 1 OR ii_sentido = 3 Then
	
	If ib_Conectado Then
		DISCONNECT USING SQLTRA;
		DESTROY SQLTRA
	End If
	
	ib_Conectado	=	False
	SQLTRA			=	Create Transaction
	
	ls_Usuario		=	sqlca.UserId
	ls_Password		=	sqlca.DbPass
	
	SELECT cone_nomodb, cone_nomser, cone_nombas,
			 cone_nodbms, cone_nomusu, cone_passwo  
	 INTO :ls_nomodb,  :ls_nomser,  :ls_nombas,
			:ls_nodbms,  :ls_Usuario, :ls_Password
	 FROM dbo.prodconectividad   
	WHERE cone_codigo = :ai_planta;
	
	SQLTRA.ServerName	=	ls_nomser
	SQLTRA.DataBase		=	ls_nombas
	SQLTRA.Dbms			= 	ls_nodbms
	SQLTRA.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
									";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
	CONNECT USING SQLTRA;
	
	If SQLTRA.SQLCode = -1 Then
		MessageBox("Problemas con Conexion","Imposible conectar a Planta Destino, se enviara correo con Archivos Planos", Exclamation!)
	Else
		ib_Conectado	=	True
		dw_movtoEncaTrans.SetTransObject(SQLTRA)
		dw_movtoDetaTrans.SetTransObject(SQLTRA)
		dw_loteEncaTrans.SetTransObject(SQLTRA)
		dw_loteDetaTrans.SetTransObject(SQLTRA)
		dw_envaEncaTrans.SetTransObject(SQLTRA)
		dw_envaDetaTrans.SetTransObject(SQLTRA)
		dw_granpesaTrans.SetTransObject(SQLTRA)
		dw_movtoBinsTrans.SetTransObject(SQLTRA)
		dw_binsTrans.SetTransObject(SQLTRA)
	End If
	
ElseIf Not IsValid(SQLTRA) Then
	
	SQLTRA			=	Create Transaction
	ib_Conectado	=	False
	
	ls_Usuario		=	sqlca.UserId
	ls_Password		=	sqlca.DbPass
	
	SELECT cone_nomodb,cone_nomser,cone_nombas,
			cone_nodbms,cone_nomusu,cone_passwo  
	 INTO :ls_nomodb,:ls_nomser,:ls_nombas,
			:ls_nodbms,:ls_Usuario,:ls_Password
	 FROM dbo.prodconectividad   
	WHERE cone_codigo = :ai_planta;
	
	SQLTRA.ServerName	=	ls_nomser
	SQLTRA.DataBase	=	ls_nombas
	SQLTRA.Dbms			= 	ls_nodbms
	SQLTRA.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
									";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
	CONNECT USING SQLTRA;
	
	If SQLTRA.SQLCode = -1 Then
		MessageBox("Problemas con Conexion","Imposible conectar a Planta Destino", Exclamation!)
	Else
		ib_Conectado	=	True
		dw_movtoEncaTrans.SetTransObject(SQLTRA)
		dw_movtoDetaTrans.SetTransObject(SQLTRA)
		dw_loteEncaTrans.SetTransObject(SQLTRA)
		dw_loteDetaTrans.SetTransObject(SQLTRA)
		dw_envaEncaTrans.SetTransObject(SQLTRA)
		dw_envaDetaTrans.SetTransObject(SQLTRA)
		dw_granpesaTrans.SetTransObject(SQLTRA)
		dw_movtoBinsTrans.SetTransObject(SQLTRA)
		dw_binsTrans.SetTransObject(SQLTRA)
		
		TriggerEvent("ue_recuperadatos")
	End If
	
End If

Return ib_conectado
end function

private function boolean of_generaarchivos ();Boolean	lb_retorno = True
String	ls_ruta, ls_archivo
Integer	ll_fila

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

ls_archivo = String(ii_planta, '0000') + String(ii_cliente, '000') + String(ii_Movto, '0000000') 

dw_archivos.InsertRow(0)

ll_fila = dw_movtoEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MovEnc.txt", Text!, False)

If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[1] = ls_archivo + "MovEnc.txt"
	dw_archivos.InsertRow(0)
End If

ll_fila = dw_movtoDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MovDet.txt", Text!, False)
If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[2] = ls_archivo + "MovDet.txt"
	dw_archivos.InsertRow(0)
End If

ll_fila = dw_loteEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "LotEnc.txt", Text!, False)
If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[3] = ls_archivo + "LotEnc.txt"
	dw_archivos.InsertRow(0)
End If

ll_fila = dw_loteDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "LotDet.txt", Text!, False)
If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[4] = ls_archivo + "LotDet.txt"
	dw_archivos.InsertRow(0)
End If

ll_fila = dw_envaEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "EnvEnc.txt", Text!, False)
If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[5] = ls_archivo + "EnvEnc.txt"
	dw_archivos.InsertRow(0)
End If

ll_fila = dw_envaDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "EnvDet.txt", Text!, False)
If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[6] = ls_archivo + "EnvDet.txt"
	dw_archivos.InsertRow(0)
End If

ll_fila = dw_movtobinsTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MvtBin.txt", Text!, False)
If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[7] = ls_archivo + "MvtBin.txt"
	dw_archivos.InsertRow(0)
End If

ll_fila = dw_granpesaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "GrnPes.txt", Text!, False)
If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[8] = ls_archivo + "GrnPes.txt"
	dw_archivos.InsertRow(0)
End If

ll_fila = dw_binsTrans.SaveAs(ls_ruta + "\" + ls_archivo + "Bins.txt", Text!, False)
If ll_fila = 1 Then 
	dw_archivos.Object.Nombre[9] = ls_archivo + "Bins.txt"
End If

If dw_archivos.RowCount() = 9 Then
	dw_archivos.SaveAs(ls_ruta + "\" + ls_archivo + "Indice.txt", Text!, False)
Else
	Messagebox("Error", "No se pudieron generar los archivos Necesarios para el Proceso", Exclamation!)	
	lb_retorno	=False
End If

Return lb_retorno
end function

private function boolean of_cargaarchivosplanos ();Boolean	lb_retorno = True
String	ls_ruta, ls_archivo, ls_RutaActual
Integer	li_ret, li_fila
Long		ll_planta

ls_RutaActual	=	GetCurrentDirectory()

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
li_ret	= 	GetFileOpenName("Seleccione Archivo", ls_ruta , ls_archivo, "txt", "Archivos Indice (*Indice.txt),*Indice.txt", ls_ruta, 18)

ChangeDirectory (ls_RutaActual)
ls_ruta 	=	Left(ls_ruta, Len(ls_ruta) - (Len(ls_archivo) + 1))

If li_ret = 0 Then //Usuario Cancela la Carga del Directorio
	lb_retorno = False
ElseIf li_ret = -1 OR NOT DirectoryExists(ls_ruta) Then 
	//Retorno error o el directorio no Existe
	MessageBox("Error", "No se pudo cargar el Directorio o Este no es Valido")
	lb_retorno = False
Else
	//Se Procede a la Carga de Archivos Planos
	//00010810000626Indice.txt
	ii_plantaorigen	=	Integer(Left(ls_archivo, 4)	)
	ii_cliente			=	Integer( Mid(ls_archivo, 5, 3))
	ii_movto			=	Integer( Mid(ls_archivo, 8, 7))
	
	ls_archivo 	= String(ii_plantaorigen, '0000') + String(ii_cliente, '000') + String(ii_Movto, '0000000')
	
	li_ret =	dw_archivos.ImportFile(ls_ruta + "\" + ls_archivo + "Indice.txt")
	
	If li_ret < 1 Then//Error con el archivo indice
		MessageBox("Error", "El Archivo " + ls_archivo + "Indice.txt" + " no Existe o no posee datos Validos")		
	Else
		If dw_archivos.RowCount() <> 9 Then
			//Valida que traiga los 9 nombres de archivos
			MessageBox("Error", "No se encontraron todos los nombres de Archivos Correspondientes al Movimiento.")
		ElseIf NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[1]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[2]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[3]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[4]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[5]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[6]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[7]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[8]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[9]) Then
				 //Valida existencia de los archivos a cargar
			MessageBox("Error", "No se encuentran todos los archivos necesarios para la importación de datos.")
			lb_retorno = False
		Else
			FOR li_fila = 1 TO dw_archivos.RowCount()
				Choose Case dw_archivos.Object.Nombre[li_fila]
					Case ls_archivo + "MovEnc.txt"
						li_ret = dw_movtoEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Encabezado del Movimiento")
							lb_retorno = False
							Exit
						End If
						
					Case ls_archivo + "MovDet.txt"
						li_ret = dw_movtoDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Detalle del Movimiento")
							lb_retorno = False
							Exit
						End If
						
					Case ls_archivo + "LotEnc.txt"
						li_ret = dw_loteEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Encabezado de los Lotes")
							lb_retorno = False
							Exit
						End If
						
					Case ls_archivo + "LotDet.txt"
						li_ret = dw_loteDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Detalle de los Lotes")
							lb_retorno = False
							Exit
						End If
						
					Case ls_archivo + "EnvEnc.txt"
						li_ret = dw_envaEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Encabezado de Envases")
							lb_retorno = False
							Exit
						End If
						
					Case ls_archivo + "EnvDet.txt"
						li_ret = dw_envaDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Detalle de Envases")
							lb_retorno = False
							Exit
						End If
						
					Case ls_archivo + "MvtBin.txt"
						li_ret = dw_movtoBinsTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Movimientos de Bins/Base Pallets")
							lb_retorno = False
							Exit
						End If	
						
					Case ls_archivo + "GrnPes.txt"
						li_ret = dw_granpesaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Detalle de Pesajes")
							lb_retorno = False
							Exit
						End If
						
					Case ls_archivo + "Bins.txt"
						li_ret = dw_binsTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						If li_ret < 1 Then
							MessageBox("Error", "No se pudo Cargar Tabla de Bins")
							lb_retorno = False
							Exit
						End If
				End Choose
			NEXT
		End If
	End If
End If

Return lb_retorno
end function

private function boolean of_cargadatos_trans ();Integer	li_fila, li_estado
Boolean 	lb_retorno = True, lb_estado = True

If dw_movtoEncaTrans.RowsCopy(1, dw_movtoEncaTrans.RowCount(), Primary!, dw_MovtoEnca, 1, Primary!) = -1 AND lb_estado Then
	MessageBox("Error al cargar datos", "Se ha producido un error al cargar la datawindow dw_movtoEncaTrans", StopSign!)
	lb_estado 	= False
	lb_retorno 	= False
End If

If dw_movtoDetaTrans.RowsCopy(1, dw_movtoDetaTrans.RowCount(), Primary!, dw_movtoDeta, 1, Primary!) = -1 AND lb_estado Then
	MessageBox("Error al cargar datos", "Se ha producido un error al cargar la tabla dw_movtoDetaTrans", StopSign!)
	lb_estado 	= False
	lb_retorno 	= False
End If

If dw_loteEncaTrans.RowsCopy(1,  dw_loteEncaTrans.RowCount(), Primary!, dw_loteEnca, 1, Primary!) = -1 AND lb_estado Then
	MessageBox("Error al cargar datos", "Se ha producido un error al cargar la tabla dw_loteEncaTrans", StopSign!)
	lb_estado 	= False
	lb_retorno 	= False
End If

If dw_loteDetaTrans.RowsCopy(1,  dw_loteDetaTrans.RowCount(), Primary!, dw_loteDeta, 1, Primary!) = -1 AND lb_estado Then
	MessageBox("Error al cargar datos", "Se ha producido un error al cargar la tabla dw_loteDetaTrans", StopSign!)
	lb_estado 	= False
	lb_retorno 	= False
End If

If dw_envaEncaTrans.RowsCopy(1,  dw_envaEncaTrans.RowCount(), Primary!, dw_envaEnca, 1, Primary!) = -1 AND lb_estado Then
	MessageBox("Error al cargar datos", "Se ha producido un error al cargar la tabla dw_envaEncaTrans", StopSign!)
	lb_estado 	= False
	lb_retorno 	= False
End If

If dw_envaDetaTrans.RowsCopy(1,  dw_envaDetaTrans.RowCount(), Primary!, dw_envaDeta, 1, Primary!) = -1 AND lb_estado Then
	MessageBox("Error al cargar datos", "Se ha producido un error al cargar la tabla dw_envaDetaTrans", StopSign!)
	lb_estado 	= False
	lb_retorno 	= False
End If

If dw_movtoBinsTrans.RowsCopy(1, dw_movtoBinsTrans.RowCount(), Primary!, dw_movtoBins, 1, Primary!) = -1 AND lb_estado Then
	MessageBox("Error al cargar datos", "Se ha producido un error al cargar la tabla dw_movtoBinsTrans", StopSign!)
	lb_estado 	= False
	lb_retorno 	= False
	
End If

If dw_granpesaTrans.RowsCopy(1,  dw_granpesaTrans.RowCount(), Primary!, dw_granpesa, 1, Primary!) = -1 AND lb_estado Then
	MessageBox("Error al cargar datos", "Se ha producido un error al cargar la tabla dw_granpesaTrans", StopSign!)
	lb_estado 	= False
	lb_retorno 	= False
End If

If lb_estado Then
	If dw_movtoEnca.RowCount() > 0 Then
		ii_plantaorigen 											= 	dw_movtoEnca.Object.plde_codigo[1]
		dw_movtoEnca.Object.mfge_estmov[1]			=	1
	End If

	FOR li_fila = 1 TO dw_movtoEnca.RowCount()
		dw_movtoEnca.Object.plde_codigo[li_fila] 			= 	ii_planta

		li_estado = dw_movtoEnca.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_movtoDeta.RowCount()
		dw_movtoDeta.Object.plde_codigo[li_fila] 			=	ii_planta
		dw_movtoDeta.Object.cama_codigo[li_fila] 			=	0

		li_estado = dw_movtoDeta.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_loteEnca.RowCount()		
		li_estado = dw_loteEnca.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_loteDeta.RowCount()
		li_estado = dw_loteDeta.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_envaEnca.RowCount()
		dw_envaEnca.Object.plde_codigo[li_fila] 			= 	ii_planta
		li_estado = dw_envaEnca.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_envaDeta.RowCount()
		dw_envaDeta.Object.plde_codigo[li_fila] 			= 	ii_planta
		dw_envaDeta.Object.fgme_sentid[li_fila] 			= 	1
		li_estado = dw_envaDeta.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_movtoBins.RowCount()
		dw_movtoBins.Object.plde_codigo[li_fila] 			= 	ii_planta
		li_estado = dw_movtoBins.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_movtoBinsTrans.RowCount()
		dw_movtoBinsTrans.Object.plde_codigo[li_fila] 	= 	ii_planta
		dw_movtobinsTrans.Object.cama_codigo[li_fila]	= 	0
		dw_movtobinsTrans.Object.fgmb_calle[li_fila] 		= 	0
		dw_movtobinsTrans.Object.fgmb_base[li_fila] 		= 	0
		dw_movtobinsTrans.Object.fgmb_posici[li_fila]		= 	0
		li_estado = dw_movtoBinsTrans.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_granpesa.RowCount()
		dw_granpesa.Object.plde_codigo[li_fila] 			= 	ii_planta
		li_estado = dw_granpesa.SetItemStatus(li_fila, 0, Primary!, NewModIfied!)
		If li_estado = -1 Then lb_retorno = False
	NEXT
End If

If lb_retorno Then
	lb_retorno = NOT iuo_movto.existe(ii_planta, ii_cliente, 2, ii_movto, TRUE, sqlca)
End If

If lb_retorno Then lb_Retorno = ValidaLotes()

Return lb_retorno
end function

event constructor;iuo_categoria							=	Create uo_categorias
iuo_frio									=	Create uo_periodofrio
iuo_condicion							=	Create uo_condicioncc	
iuo_variedad							=	Create uo_variedades
iuo_envases								=	Create uo_envases
iuo_camion								=	Create uo_camiones
iuo_especie								=	Create uo_especie
iuo_planta								=	Create uo_plantadesp
iuo_productor							=	Create uo_productores
iuo_cliente								=	Create uo_Clientesprod
iuo_motivo								=	Create uo_motivodespacho
iuo_transp								=	Create uo_transportista
iuo_bins									=	Create uo_bins
iuo_camarasbode						=	Create uo_camarasfrigo
iuo_movto								=	Create uo_spro_movtofrutagranenca
iuo_lotes									=	Create uo_lotesfrutagranel

dw_archivos								=	Create DataStore
dw_envaencatrans						=	Create DataStore 
dw_loteencatrans						=	Create DataStore 
dw_movtoencatrans					=	Create DataStore
dw_movtodetatrans					=	Create DataStore 
dw_lotedetatrans						=	Create DataStore 
dw_envadetatrans						=	Create DataStore 
dw_granpesatrans						=	Create DataStore 
dw_granpesa							=	Create DataStore
dw_envadeta							=	Create DataStore 
dw_lotedeta								=	Create DataStore 
dw_movtodeta							=	Create DataStore
dw_movtoenca							=	Create DataStore
dw_loteenca							=	Create DataStore
dw_envaenca							=	Create DataStore
dw_movtobins							=	Create DataStore
dw_movtobinsTrans					= 	Create DataStore
dw_binstrans							= 	Create DataStore
dw_bins									= 	Create DataStore

dw_movtoencatrans.dataobject 	= "dw_mant_movtofrutagranenca_trans"
dw_movtodetatrans.dataobject 	= "dw_mues_movtofrutagraneldeta_trans"
dw_envaencatrans.dataobject 		= "dw_mant_movtoenvaenca_trans"
dw_envadetatrans.dataobject 		= "dw_mues_movtoenvadeta_trans"
dw_loteencatrans.dataobject 		= "dw_mues_spro_lotesfrutagranel_trans"
dw_lotedetatrans.dataobject 		= "dw_mues_spro_lotesfrutagradet_trans"
dw_granpesatrans.dataobject 		= "dw_pesaje_romana_trans"
dw_movtobinstrans.dataobject 		= "dw_spro_movtobins_trans"
dw_binstrans.dataobject 			= "dw_mues_spro_bins_trans"

dw_archivos.dataobject 				= "dw_archivos"

dw_movtoenca.dataobject 			= "dw_mant_movtofrutagranenca_origen"
dw_movtodeta.dataobject 			= "dw_mues_movtofrutagraneldeta_origen"
dw_envaenca.dataobject 			= "dw_mant_movtoenvaenca_origen"
dw_envadeta.dataobject 			= "dw_mues_movtoenvadeta_origen"
dw_loteenca.dataobject 				= "dw_mues_spro_lotesfrutagranel_origen"
dw_lotedeta.dataobject 				= "dw_mues_spro_lotesfrutagradet_origen"
dw_granpesa.dataobject 			= "dw_pesaje_romana_origen"
dw_movtobins.dataobject 			= "dw_spro_movtobins_origen"
dw_bins.dataobject 					= "dw_mues_spro_bins_origen"
End event

on nvuo_traspaso_interplanta.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nvuo_traspaso_interplanta.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event destructor;Destroy dw_archivos
Destroy dw_envaencatrans
Destroy dw_loteencatrans
Destroy dw_movtoencatrans
Destroy dw_movtodetatrans
Destroy dw_lotedetatrans
Destroy dw_envadetatrans
Destroy dw_granpesatrans
Destroy dw_granpesa
Destroy dw_envadeta
Destroy dw_lotedeta
Destroy dw_movtodeta
Destroy dw_movtoenca
Destroy dw_loteenca
Destroy dw_envaenca
Destroy dw_movtobins
Destroy dw_movtobinsTrans
Destroy dw_binstrans
Destroy dw_bins

Destroy iuo_categoria
Destroy iuo_frio
Destroy iuo_condicion		
Destroy iuo_variedad		
Destroy iuo_envases			
Destroy iuo_camion	
Destroy iuo_especie	
Destroy iuo_planta	
Destroy iuo_productor
Destroy iuo_cliente
Destroy iuo_motivo	
Destroy iuo_transp	
Destroy iuo_bins		
Destroy iuo_camarasbode
Destroy iuo_movto		
Destroy iuo_lotes
end event

