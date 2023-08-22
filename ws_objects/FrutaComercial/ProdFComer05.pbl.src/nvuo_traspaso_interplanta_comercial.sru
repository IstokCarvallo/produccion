$PBExportHeader$nvuo_traspaso_interplanta_comercial.sru
forward
global type nvuo_traspaso_interplanta_comercial from nonvisualobject
end type
end forward

global type nvuo_traspaso_interplanta_comercial from nonvisualobject
event traspasadatos ( )
event ue_recuperadatos ( )
event ue_guardar ( )
event ue_borrar ( )
end type
global nvuo_traspaso_interplanta_comercial nvuo_traspaso_interplanta_comercial

type variables
Transaction							SQLTRA
Boolean								ib_Conectado, lb_LocalRemoto

Integer								ii_Planta, ii_Tipo, ii_Movto, ii_Cliente, ii_sentido,&
										ii_ManAut, retorno, ii_plantades, ii_plantaorigen
										
Long									il_guiasii

Window								w_parent				

uo_categorias						iuo_categoria
uo_periodofrio						iuo_frio
//uo_condicioncc					iuo_condicion
uo_variedades						iuo_variedad
uo_envases							iuo_envases
uo_camiones							iuo_camion
uo_especie							iuo_especie
uo_plantadesp						iuo_planta
uo_productores						iuo_productor
uo_clientesprod					iuo_cliente
uo_motivodespacho					iuo_motivo
uo_transportista					iuo_transp
uo_bins								iuo_bins
uo_camarasbode						iuo_camarasbode
uo_spro_lotesfrutacomenc		iuo_Lotes
uo_lotesfrutacomer				iuo_LotCom

DataStore							dw_archivos,dw_envaencatrans,dw_loteencatrans,dw_movtoencatrans,		&
										dw_movtodetatrans,dw_lotedetatrans,dw_envadetatrans,dw_granpesatrans,&
										dw_granpesa,dw_envadeta,dw_lotedeta,dw_movtodeta,							&
										dw_movtoenca,dw_loteenca,dw_envaenca, dw_movtobins,dw_movtobinstrans,&
										dw_binstrans,dw_bins
end variables

forward prototypes
public function boolean cargadatos ()
public function boolean conectatrans (integer ai_planta)
public function boolean wf_actualiza_db (boolean borrando)
public function boolean generaarchivos ()
public function boolean cargaarchivosplanos ()
public function boolean cargadatos_trans ()
public function boolean validalotes ()
public function boolean validamovto ()
public function boolean validaenvases ()
public subroutine enviamail ()
public function boolean validabins ()
public function boolean wf_transamite ()
public function boolean wf_errores (integer tipo, string titulo)
public function boolean wf_validamovto (integer ai_tipo, long al_numero, transaction at_transaccion)
end prototypes

event traspasadatos();Str_Busqueda	lstr_busq

dw_movtoEnca.SetTransObject(sqlca)
dw_movtoDeta.SetTransObject(sqlca)
dw_loteEnca.SetTransObject(sqlca)
dw_loteDeta.SetTransObject(sqlca)
dw_envaEnca.SetTransObject(sqlca)
dw_envaDeta.SetTransObject(sqlca)
dw_movtobins.SetTransObject(sqlca)
dw_bins.SetTransObject(sqlca)

dw_movtoEncaTrans.Reset()
dw_movtoDetaTrans.Reset()
dw_loteEncaTrans.Reset()
dw_loteDetaTrans.Reset()
dw_envaEncaTrans.Reset()
dw_envaDetaTrans.Reset()
dw_movtoBinsTrans.Reset()
dw_binsTrans.Reset()

IF ii_sentido = 1 THEN
	
	This.TriggerEvent("ue_RecuperaDatos")
	
	IF Message.DoubleParm <> -1 THEN
		IF Cargadatos() THEN
			This.Triggerevent("ue_guardar")
			
		ELSE
			MessageBox("Error", "La carga de datos presento problemas")
			
		END IF
		
	ELSE
		MessageBox("Error", "Imposible regenerar datos de Despacho")
		
	END IF
ELSE
	
	IF ii_ManAut = 2 THEN
	
		lstr_Busq.Argum[1]	=	String(ii_planta)
		lstr_Busq.Argum[2]	=	String(ii_tipo)
		lstr_Busq.Argum[3]	=	""
		lstr_Busq.Argum[4]	=	String(Today(), 'dd/mm/yyyy')

		OpenWithParm(w_busc_movtofrutacomenca_trans, lstr_busq)

		lstr_busq	=	Message.PowerObjectParm

		If lstr_Busq.Argum[1] <> "" Then
			ii_planta		=	Integer(lstr_Busq.Argum[1])
			ii_tipo			=	Integer(lstr_Busq.Argum[2])
			ii_movto			=	Integer(lstr_Busq.Argum[3])
			ii_cliente		=	Integer(lstr_Busq.Argum[12])

			This.TriggerEvent("ue_RecuperaDatos")
			
		END IF

	ELSE
		IF NOT CargaArchivosPlanos() THEN
			MessageBox("Error", "La carga de Archivos Planos presento problemas")
			Message.DoubleParm = -1
			
		END IF

	END IF

	IF Message.DoubleParm <> -1 THEN
		IF Cargadatos_Trans() THEN
			w_parent.TriggerEvent("ue_carga_detalle")
			
		ELSE
			MessageBox("Error", "La carga de datos presento problemas")
			
		END IF
	ELSE
		MessageBox("Error", "Imposible regenerar datos para Recepción")
		
	END IF
END IF
end event

event ue_recuperadatos();Integer	li_filas, respuesta
Boolean 	lb_retorno = True

IF ii_sentido = 1 Then
	DO
		IF dw_MovtoEnca.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_MovtoDeta.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1	OR &
 			 dw_LoteEnca.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			 dw_LoteDeta.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			 dw_EnvaEnca.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			 dw_EnvaDeta.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_movtobins.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
				  dw_bins.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 THEN

			respuesta 					= 	MessageBox("Error en Base de Datos", &
															  "No es posible conectar la Base de Datos.", &
															  Information!, RetryCancel!)
			IF respuesta = 2 THEN
				Message.DoubleParm 	= 	-1
			END IF

		END IF
	LOOP WHILE respuesta = 1

ELSE
	IF NOT ib_conectado THEN
		dw_movtoEncaTrans.SetTransObject(sqlca)
		dw_movtoDetaTrans.SetTransObject(sqlca)
		 dw_loteEncaTrans.SetTransObject(sqlca)
		 dw_loteDetaTrans.SetTransObject(sqlca)
		 dw_envaEncaTrans.SetTransObject(sqlca)
		 dw_envaDetaTrans.SetTransObject(sqlca)
		dw_movtoBinsTrans.SetTransObject(sqlca)
			  dw_binsTrans.SetTransObject(sqlca)

	END IF

	DO
		IF dw_MovtoEncaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_MovtoDetaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			 dw_LoteEncaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			 dw_LoteDetaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			 dw_EnvaEncaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			 dw_EnvaDetaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_movtoBinsTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
				  dw_binsTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 THEN

			respuesta 					= 	MessageBox("Error en Base de Datos", &
															  "No es posible conectar la Base de Datos.", &
															  Information!, RetryCancel!)
			IF respuesta = 2 THEN
				Message.DoubleParm 	= 	-1
			END IF

		ELSE
			IF NOT wf_ValidaMovto(ii_tipo, ii_movto, Sqlca) THEN
				Message.DoubleParm = -1
			END IF
		END IF
	LOOP WHILE respuesta = 1

END IF

IF (NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.bultobins) AND Message.DoubleParm <> -1 THEN
	IF ii_sentido = 1 THEN
		IF dw_movtobins.RowCount() < 1 THEN dw_movtobins.InsertRow(0)
		IF dw_bins.RowCount() < 1 THEN dw_bins.InsertRow(0)
	ELSE
		IF dw_movtobinsTrans.RowCount() < 1 THEN dw_movtobinsTrans.InsertRow(0)
		IF dw_binsTrans.RowCount() < 1 THEN dw_binsTrans.InsertRow(0)
	END IF
END IF
end event

event ue_guardar();SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

IF ConectaTrans(ii_plantades) THEN 
	w_main.SetMicroHelp("Comenzando Proceso Automatico.")	
	wf_transamite()
	
ELSE
	w_main.SetMicroHelp("Comenzando Proceso Via E-Mail")	
	IF GeneraArchivos() THEN
		EnviaMail()
		
	END IF
	
END IF
	
end event

event ue_borrar();Integer	li_status
SetPointer(HourGlass!)

w_main.SetMicroHelp("Eliminando información...")

Message.DoubleParm = 0

IF lb_LocalRemoto THEN
	IF ConectaTrans(ii_plantades) THEN
		w_main.SetMicroHelp("Comenzando Proceso de Eliminación Automatica.")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Información Eliminada.")
		ELSE
			w_main.SetMicroHelp("No se puede Eliminar información.")
			Message.DoubleParm = -1
			RETURN
		END IF
		
	ELSE
		w_main.SetMicroHelp("No se ha podido conectar a base de datos Destino")
		
	END IF
ELSE
	w_main.SetMicroHelp("Comenzando Proceso de Eliminación Automatica.")
	IF wf_actualiza_db(True) THEN
		w_main.SetMicroHelp("Información Eliminada.")
		
	ELSE
		w_main.SetMicroHelp("No se puede Eliminar información.")
		Message.DoubleParm = -1
		RETURN
		
	END IF
END IF
end event

public function boolean cargadatos ();Integer	li_fila, li_estado
Boolean 	lb_retorno = True

IF dw_movtoEnca.RowsCopy(1, dw_movtoEnca.RowCount(), &
							    Primary!,dw_MovtoEncaTrans, 1, Primary!) = -1 OR &
	dw_movtoDeta.RowsCopy(1, dw_movtoDeta.RowCount(),  &
							    Primary!,dw_movtoDetaTrans, 1, Primary!) = -1 OR &
	 dw_loteEnca.RowsCopy(1, dw_loteEnca.RowCount(),  &
							    Primary!, dw_loteEncaTrans, 1, Primary!) = -1 OR &
	 dw_loteDeta.RowsCopy(1, dw_loteDeta.RowCount(),  &
							    Primary!, dw_loteDetaTrans, 1, Primary!) = -1 OR &
	 dw_envaEnca.RowsCopy(1, dw_envaEnca.RowCount(),  &
							    Primary!, dw_envaEncaTrans, 1, Primary!) = -1 OR &
	 dw_envaDeta.RowsCopy(1, dw_envaDeta.RowCount(),  &
							    Primary!, dw_envaDetaTrans, 1, Primary!) = -1 OR &
	dw_movtobins.RowsCopy(1, dw_movtobins.RowCount(),  &
							    Primary!,dw_movtobinsTrans, 1, Primary!) = -1 OR &
	     dw_bins.RowsCopy(1, dw_bins.RowCount(),  &
							    Primary!, dw_binsTrans, 1, Primary!) = -1 THEN
	lb_retorno = False
ELSE
	FOR li_fila = 1 TO dw_movtoEncaTrans.RowCount()
		dw_movtoEncaTrans.Object.mfco_guisii[li_fila]	=	il_guiasii
		dw_movtoEncaTrans.Object.plde_codigo[li_fila]	=	ii_plantades
		
		li_estado = dw_movtoEncaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_movtoDetaTrans.RowCount()
		dw_movtoDetaTrans.Object.cama_codigo[li_fila] 	= 	0
		dw_movtoDetaTrans.Object.plde_codigo[li_fila]	=	ii_plantades
		
		li_estado = dw_movtoDetaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_loteEncaTrans.RowCount()
		
		li_estado = dw_loteEncaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_loteDetaTrans.RowCount()
		li_estado = dw_loteDetaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_envaEncaTrans.RowCount()
		dw_envaEncaTrans.Object.meen_guisii[li_fila]		=	il_guiasii
		dw_envaEncaTrans.Object.plde_codigo[li_fila]		=	ii_plantades
		
		li_estado = dw_envaEncaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_envaDetaTrans.RowCount()
		dw_envaDetaTrans.Object.plde_codigo[li_fila]		=	ii_plantades
		
		li_estado = dw_envaDetaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_movtobinsTrans.RowCount()
		//Asigno numero de despacho, para recuperar datos en destino.
		dw_movtobinsTrans.Object.mfge_numero[li_fila] 	= 	ii_movto
		dw_movtobinsTrans.Object.plde_codigo[li_fila]	=	ii_plantades
		
		//Anulo camara, calle, posición y piso para evitar problemas en destino.
		dw_movtobinsTrans.Object.cama_codigo[li_fila]	= 	0
		dw_movtobinsTrans.Object.fgmb_calle[li_fila] 	= 	0
		dw_movtobinsTrans.Object.fgmb_base[li_fila] 		= 	0
		dw_movtobinsTrans.Object.fgmb_posici[li_fila]	= 	0
		
		//Cambio estado a activo.
		dw_movtobinsTrans.Object.fgmb_estado[li_fila]	= 	2
		
		li_estado = dw_movtobinsTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT

	FOR li_fila = 1 TO dw_binsTrans.RowCount()
		dw_binsTrans.Object.plde_codigo[li_fila]			=	ii_plantades
		
		li_estado = dw_binsTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
END IF

Return lb_retorno
end function

public function boolean conectatrans (integer ai_planta);String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ii_sentido = 1 THEN
	
	IF ib_Conectado THEN
		DISCONNECT USING SQLTRA;
		DESTROY SQLTRA
	END IF
	
	SELECT cone_nomodb,cone_nomser,cone_nombas,
			 cone_nodbms,cone_nomusu,cone_passwo  
	  INTO :ls_nomodb,:ls_nomser,:ls_nombas,
			 :ls_nodbms,:ls_Usuario,:ls_Password
	  FROM dbo.prodconectividad   
	 WHERE cone_codigo = :ai_planta;
	
	ls_Usuario			=	sqlca.UserId
	ls_Password			=	sqlca.DbPass
	SQLTRA				=	Create Transaction
	SQLTRA.ServerName	=	ls_nomser
	SQLTRA.DataBase	=	ls_nombas
	SQLTRA.Dbms			= 	ls_nodbms
	SQLTRA.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
								
	CONNECT USING SQLTRA;
	
	IF SQLTRA.SQLCode = -1 THEN
		MessageBox("Problemas con Conexion","Imposible conectar a Planta Destino, se enviara correo con Archivos Planos", Exclamation!)
	ELSE
		ib_Conectado	=	True
		dw_movtoEncaTrans.SetTransObject(SQLTRA)
		dw_movtoDetaTrans.SetTransObject(SQLTRA)
		 dw_loteEncaTrans.SetTransObject(SQLTRA)
		 dw_loteDetaTrans.SetTransObject(SQLTRA)
		 dw_envaEncaTrans.SetTransObject(SQLTRA)
		 dw_envaDetaTrans.SetTransObject(SQLTRA)
		dw_movtoBinsTrans.SetTransObject(SQLTRA)
		dw_binsTrans.SetTransObject(SQLTRA)
	END IF
	
ELSEIF NOT IsValid(SQLTRA) THEN
	SELECT cone_nomodb,cone_nomser,cone_nombas,
			 cone_nodbms,cone_nomusu,cone_passwo  
	  INTO :ls_nomodb,:ls_nomser,:ls_nombas,
			 :ls_nodbms,:ls_Usuario,:ls_Password
	  FROM dbo.prodconectividad   
	 WHERE cone_codigo = :ai_planta;
	 
	ib_Conectado		=	False
	ls_Usuario			=	sqlca.UserId
	ls_Password			=	sqlca.DbPass
	SQLTRA				=	Create Transaction
	SQLTRA.ServerName	=	ls_nomser
	SQLTRA.DataBase	=	ls_nombas
	SQLTRA.Dbms			= 	ls_nodbms
	SQLTRA.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
	CONNECT USING SQLTRA;
	
	IF SQLTRA.SQLCode = -1 THEN
		MessageBox("Problemas con Conexion","Imposible conectar a Planta Destino", Exclamation!)
		
	ELSE
		ib_Conectado	=	True
		
		dw_movtoEncaTrans.SetTransObject(SQLTRA)
		dw_movtoDetaTrans.SetTransObject(SQLTRA)
		 dw_loteEncaTrans.SetTransObject(SQLTRA)
		 dw_loteDetaTrans.SetTransObject(SQLTRA)
		 dw_envaEncaTrans.SetTransObject(SQLTRA)
		 dw_envaDetaTrans.SetTransObject(SQLTRA)
		dw_movtoBinsTrans.SetTransObject(SQLTRA)
		dw_binsTrans.SetTransObject(SQLTRA)
		
		TriggerEvent("ue_recuperadatos")
	END IF
	
END IF

Return ib_conectado
end function

public function boolean wf_actualiza_db (boolean borrando);Boolean 		lb_retorno
Transaction	lt_trans

lt_trans		=	Create Transaction

IF NOT IsValid(SQLTra) THEN
	lt_trans	=	SQLCa
	
ELSE
	lt_trans	=	SQLTra
	
END IF
	
IF borrando THEN
	IF dw_movtoencatrans.RowCount() > 0 THEN
		DO
			IF dw_movtoencatrans.DeleteRow(1) = -1 THEN RETURN FALSE
		LOOP WHILE dw_movtoencatrans.RowCount() > 0
	END IF

	IF dw_movtodetatrans.RowCount() > 0 THEN
		DO
			IF dw_movtodetatrans.DeleteRow(1) = -1 THEN RETURN FALSE
		LOOP WHILE dw_movtodetatrans.RowCount() > 0
	END IF
	
	IF dw_movtobinstrans.RowCount() > 0 THEN
		DO
			IF dw_movtobinstrans.DeleteRow(1) = -1 THEN RETURN FALSE
		LOOP WHILE dw_movtobinstrans.RowCount() > 0
	END IF

	IF dw_envaencatrans.RowCount() > 0 THEN
		DO
			IF dw_envaencatrans.DeleteRow(1) = -1 THEN RETURN FALSE
		LOOP WHILE dw_envaencatrans.RowCount() > 0
	END IF
	 
	IF dw_envadetatrans.RowCount() > 0 THEN
		DO
			IF dw_envadetatrans.DeleteRow(1) = -1 THEN RETURN FALSE
		LOOP WHILE dw_envadetatrans.RowCount() > 0
	END IF

	IF dw_loteencatrans.RowCount() > 0 THEN
		DO
			IF dw_loteencatrans.DeleteRow(1) = -1 THEN RETURN FALSE
		LOOP WHILE dw_loteencatrans.RowCount() > 0
	END IF
	
	IF dw_lotedetatrans.RowCount() > 0 THEN
		DO
			IF dw_lotedetatrans.DeleteRow(1) = -1 THEN RETURN FALSE
		LOOP WHILE dw_lotedetatrans.RowCount() > 0
	END IF
	
	IF IsValid(dw_granpesaTrans) AND Not IsNull(dw_granpesaTrans) THEN
		IF dw_granpesaTrans.RowCount() > 0 THEN
			DO
				IF dw_granpesaTrans.DeleteRow(1) = -1 THEN RETURN FALSE
			LOOP WHILE dw_granpesaTrans.RowCount() > 0
		END IF
	END IF
	
	IF dw_movtobinsTrans.RowCount() > 0 THEN
		Do
			IF dw_movtobinsTrans.DeleteRow(1) = -1 THEN RETURN FALSE
		LOOP WHILE dw_movtobinsTrans.RowCount() > 0
	END IF
END IF

IF dw_movtoEncaTrans.Update(True, False) > -1 THEN		 						//Encabezado
	IF dw_loteEncaTrans.Update(True, False) > -1 THEN							//Lotes
		IF dw_loteDetaTrans.Update(True, False) > -1 THEN						//Detalle de Lotes
			IF dw_movtoDetaTrans.Update(True, False) > -1 THEN					//Detalle
				IF dw_envaEncaTrans.Update(True,False) > -1 THEN				//Envases Encabezado
					IF dw_envaDetaTrans.Update(True,False) > -1 THEN			//Envases Recepcionados
						IF dw_granpesaTrans.Update(True,False) > -1 THEN		//Detalle Pesaje
							IF dw_movtoBinsTrans.Update(True,False) > -1 THEN	//Movimiento Bins
								IF dw_binsTrans.Update(True,False) > -1 THEN 	//Movimiento Bins
									Commit;
									
									IF lt_Trans.SQLCode <> 0 THEN
										F_ErrorBaseDatos(lt_Trans, "Commit")
									ELSE
										lb_Retorno	=	True
									
										dw_movtoEncaTrans.ResetUpdate()
										dw_movtoDetaTrans.ResetUpdate()
										 dw_loteEncaTrans.ResetUpdate()
										 dw_loteDetaTrans.ResetUpdate()
										 dw_envaEncaTrans.ResetUpdate()
										 dw_envaDetaTrans.ResetUpdate()
										 dw_granpesaTrans.ResetUpdate()
										dw_movtoBinsTrans.ResetUpdate()
										
									END IF
								ELSE
									F_ErrorBaseDatos(lt_Trans, "Movimiento Bins")//Movimiento Bins
									RollBack;
								END IF
							ELSE
								F_ErrorBaseDatos(lt_Trans, "Movimiento Bins")	//Movimiento Bins
								RollBack;
							END IF
						ELSE
							F_ErrorBaseDatos(lt_Trans, "Detalle Pesaje")			//Detalle Pesaje
							RollBack;
						END IF
					ELSE
						F_ErrorBaseDatos(lt_Trans, "Envases Recepcionados")	//Envases Recepcionados
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(lt_Trans, "Envases Encabezado")			//Envases Encabezado
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(lt_Trans, "Detalle Movto")						//Detalle
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(lt_Trans, "Detalle de Lotes")						//Detalle de Lotes
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(lt_Trans, "Encabezado Lotes")							//Lotes
		RollBack;
	END IF
ELSE
	F_ErrorBaseDatos(lt_Trans, "Encabezado Movimiento")						//Encabezado Movimiento
	RollBack;
END IF

Return lb_retorno
end function

public function boolean generaarchivos ();Boolean	lb_retorno = True
String	ls_ruta, ls_archivo
Integer	ll_fila

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

ls_archivo = String(ii_planta, '0000') + String(ii_cliente, '000') + String(ii_Movto, '0000000') 

dw_archivos.InsertRow(0)

ll_fila = dw_movtoEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MovEnc_Comercial.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[1] = ls_archivo + "MovEnc_Comercial.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_movtoDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MovDet_Comercial.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[2] = ls_archivo + "MovDet_Comercial.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_loteEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "LotEnc_Comercial.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[3] = ls_archivo + "LotEnc_Comercial.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_loteDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "LotDet_Comercial.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[4] = ls_archivo + "LotDet_Comercial.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_envaEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "EnvEnc_Comercial.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[5] = ls_archivo + "EnvEnc_Comercial.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_envaDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "EnvDet_Comercial.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[6] = ls_archivo + "EnvDet_Comercial.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_movtobinsTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MvtBin_Comercial.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[7] = ls_archivo + "MvtBin_Comercial.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_binsTrans.SaveAs(ls_ruta + "\" + ls_archivo + "Bins_Comercial.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[8] = ls_archivo + "Bins_Comercial.txt"
END IF

IF dw_archivos.RowCount() = 8 THEN
	dw_archivos.SaveAs(ls_ruta + "\" + ls_archivo + "Indice_Comercial.txt", Text!, False)
ELSE
	Messagebox("Error", "No se pudieron generar los archivos Necesarios para el Proceso", Exclamation!)
	
	lb_retorno	=False
END IF

Return lb_retorno
end function

public function boolean cargaarchivosplanos ();Boolean	lb_retorno = True
String	ls_ruta, ls_archivo, ls_RutaActual
Integer	li_ret, li_fila

ls_RutaActual	=	GetCurrentDirectory()

RegistryGet("HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

li_ret	= 	GetFileOpenName("Seleccione Archivo", ls_ruta , ls_archivo, "txt", + &
								    "Archivos Indice (*Indice_Comercial.txt),*Indice_Comercial.txt", ls_ruta, 18)

ChangeDirectory (ls_RutaActual)
ls_ruta 	=	Left(ls_ruta, Len(ls_ruta) - (Len(ls_archivo) + 1))

IF li_ret = 0 THEN //Usuario Cancela la Carga del Directorio
	lb_retorno = False
	
ELSEIF li_ret = -1 OR NOT DirectoryExists(ls_ruta) THEN 
	//Retorno error o el directorio no Existe
	MessageBox("Error", "No se pudo cargar el Directorio o Este no es Valido")
	lb_retorno = False
	
ELSE
	//Se Procede a la Carga de Archivos Planos
	//00010810000626Indice_Comercial.txt
	ii_plantaorigen	=	Integer(Mid(ls_archivo, 1, 4))
	ii_cliente			=	Integer(Mid(ls_archivo, 5, 3))
	ii_movto				=	Integer(Mid(ls_archivo, 8, 7))

	ls_archivo 	= 	String(ii_plantaorigen, '0000') + String(ii_cliente, '000') + String(ii_Movto, '0000000') 
	li_ret 		=	dw_archivos.ImportFile(ls_ruta + "\" + ls_archivo + "Indice_Comercial.txt")
										  			  //00010810000111Indice_Comercial.txt

	IF li_ret < 1 THEN//Error con el archivo indice
		MessageBox("Error", "El Archivo " + ls_archivo + "Indice_Comercial.txt no Existe o no posee datos Validos")
		
	ELSE

		IF dw_archivos.RowCount() <> 8 THEN
			//Valida que traiga los 8 nombres de archivos
			MessageBox("Error", "No se encontraron todos los nombres de Archivos Correspondientes al Movimiento.")
			
		ELSEIF NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[1]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[2]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[3]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[4]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[5]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[6]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[7]) OR &
				 NOT FileExists(ls_ruta + "\" + dw_archivos.Object.Nombre[8]) THEN

			//Valida existencia de los archivos a cargar
			MessageBox("Error", "No se encuentran todos los archivos necesarios para la importación de datos.")
			lb_retorno = False

		ELSE
			FOR li_fila = 1 TO dw_archivos.RowCount()

				CHOOSE CASE dw_archivos.Object.Nombre[li_fila]
					CASE ls_archivo + "MovEnc_Comercial.txt"
						li_ret = dw_movtoEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])

						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Encabezado del Movimiento")
							lb_retorno = False
							Exit
						END IF

					CASE ls_archivo + "MovDet_Comercial.txt"
						li_ret = dw_movtoDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])

						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Detalle del Movimiento")
							lb_retorno = False
							Exit
						END IF

					CASE ls_archivo + "LotEnc_Comercial.txt"
						li_ret = dw_loteEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])

						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Encabezado de los Lotes")
							lb_retorno = False
							Exit
						END IF

					CASE ls_archivo + "LotDet_Comercial.txt"
						li_ret = dw_loteDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Detalle de los Lotes")
							lb_retorno = False
							Exit
						END IF

					CASE ls_archivo + "EnvEnc_Comercial.txt"
						li_ret = dw_envaEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Encabezado de Envases")
							lb_retorno = False
							Exit
						END IF

					CASE ls_archivo + "EnvDet_Comercial.txt"
						li_ret = dw_envaDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])

						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Detalle de Envases")
							lb_retorno = False
							Exit
						END IF

					CASE ls_archivo + "MvtBin_Comercial.txt"
						li_ret = dw_movtoBinsTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])

						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Movimientos de Bins/Base Pallets")
							lb_retorno = False
							Exit
						END IF	

					CASE ls_archivo + "Bins_Comercial.txt"
						li_ret = dw_binsTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])

						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Tabla de Bins")
							lb_retorno = False
							Exit
						END IF

				END CHOOSE
			NEXT
		END IF
	END IF
END IF

Return lb_retorno
end function

public function boolean cargadatos_trans ();Integer	li_fila, li_estado
Boolean 	lb_retorno = True

IF dw_movtoEncaTrans.RowsCopy(1, dw_movtoEncaTrans.RowCount(),	Primary!, dw_MovtoEnca, 1, Primary!) = -1 OR &
	dw_movtoDetaTrans.RowsCopy(1, dw_movtoDetaTrans.RowCount(),	Primary!, dw_movtoDeta, 1, Primary!) = -1 OR &
	 dw_loteEncaTrans.RowsCopy(1, dw_loteEncaTrans.RowCount(), 	Primary!, dw_loteEnca, 	1, Primary!) = -1 OR &
	 dw_loteDetaTrans.RowsCopy(1, dw_loteDetaTrans.RowCount(), 	Primary!, dw_loteDeta, 	1, Primary!) = -1 OR &
	 dw_envaEncaTrans.RowsCopy(1, dw_envaEncaTrans.RowCount(), 	Primary!, dw_envaEnca, 	1, Primary!) = -1 OR &
	 dw_envaDetaTrans.RowsCopy(1, dw_envaDetaTrans.RowCount(),  Primary!, dw_envaDeta, 	1, Primary!) = -1 OR &
	dw_movtobinsTrans.RowsCopy(1, dw_movtobinsTrans.RowCount(),  Primary!, dw_movtobins, 	1, Primary!) = -1 THEN
		lb_retorno = False
ELSE
	
	IF dw_movtoEnca.RowCount() > 0 THEN
		ii_plantaorigen = dw_movtoEnca.Object.plde_codigo[1]
	END IF
	
	FOR li_fila = 1 TO dw_movtoEnca.RowCount()
		dw_movtoEnca.Object.plde_codigo[li_fila] = ii_planta
		
		li_estado = dw_movtoEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_movtoDeta.RowCount()
		dw_movtoDeta.Object.plde_codigo[li_fila] = ii_planta
		dw_movtoDeta.Object.cama_codigo[li_fila] 	= 	0
		
		li_estado = dw_movtoDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_loteEnca.RowCount()
//		dw_loteEnca.Object.lote_pltcod[li_fila] = ii_planta

		If iuo_Lotes.Existe(dw_loteEnca.Object.lofc_pltcod[li_fila], &
								  dw_loteEnca.Object.lofc_espcod[li_fila], &
								  dw_loteEnca.Object.lofc_lotefc[li_fila], False, Sqlca) THEN		
			li_estado = dw_loteEnca.SetItemStatus(li_fila, 0, Primary!, DataModified!)
			IF li_estado = -1 THEN lb_retorno 		= 	False
			
		ELSE	
			li_estado = dw_loteEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
			IF li_estado = -1 THEN lb_retorno 		= 	False
			
		End If
	NEXT
	
	FOR li_fila = 1 TO dw_loteDeta.RowCount()
//		dw_loteDeta.Object.lote_pltcod[li_fila] 	= 	ii_planta
		
		li_estado = dw_loteDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_envaEnca.RowCount()
		dw_envaEnca.Object.plde_codigo[li_fila] 	= 	ii_planta
		
		li_estado = dw_envaEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_envaDeta.RowCount()
		dw_envaDeta.Object.plde_codigo[li_fila] 	= 	ii_planta
		
		li_estado = dw_envaDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_movtoBins.RowCount()
		dw_movtoBins.Object.plde_codigo[li_fila] 	= 	ii_planta
		
		dw_movtoBins.Object.cama_codigo[li_fila]	= 	0
		dw_movtoBins.Object.fgmb_calle[li_fila] 	= 	0
		dw_movtoBins.Object.fgmb_base[li_fila] 	= 	0
		dw_movtoBins.Object.fgmb_posici[li_fila]	= 	0
		
		li_estado = dw_movtoBins.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	FOR li_fila = 1 TO dw_movtoBinsTrans.RowCount()
		dw_movtoBinsTrans.Object.plde_codigo[li_fila] = ii_planta
		
		li_estado = dw_movtoBinsTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
//	FOR li_fila = 1 TO dw_granpesa.RowCount()
//		dw_granpesa.Object.plde_codigo[li_fila] = ii_planta
//		
//		li_estado = dw_granpesa.SetItemStatus(li_fila, 0, Primary!, NewModified!)
//		IF li_estado = -1 THEN lb_retorno = False
//	NEXT
//	
//	FOR li_fila = 1 TO dw_bins.RowCount()
//		dw_bins.Object.plde_codigo[li_fila] = ii_planta
//		
//		li_estado = dw_bins.SetItemStatus(li_fila, 0, Primary!, NewModified!)
//		IF li_estado = -1 THEN lb_retorno = False
//	NEXT
	
END IF

IF lb_retorno THEN lb_Retorno = ValidaLotes()

Return lb_retorno
end function

public function boolean validalotes ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_loteencatrans.RowCount()
	
	li_find	=	dw_movtodetatrans.Find("lofc_pltcod = " + String(dw_loteencatrans.Object.lofc_pltcod[li_fila]) + " AND " +&
												  "lofc_espcod = " + String(dw_loteencatrans.Object.lofc_espcod[li_fila]) + " AND " +&
												  "lofc_lotefc = " + String(dw_loteencatrans.Object.lofc_lotefc[li_fila]) , &
												  1, dw_movtodetatrans.RowCount())
	IF li_find < 1 THEN
		MessageBox("Error", 	"El Lote " + String(dw_lotedetatrans.Object.lofc_pltcod[li_fila], '0000-')  + &
												   String(dw_lotedetatrans.Object.lofc_espcod[li_fila], '000-')   + &
												   String(dw_lotedetatrans.Object.lofc_lotefc[li_fila], '00000 ') + &
									"no pertenece a este Movimiento, no se podra generar recepción")
		lb_retorno = False
		
	ELSEIF NOT iuo_especie.Existe(dw_loteencatrans.Object.lofc_espcod[li_fila], True, sqlca) THEN
		lb_retorno = False
		
	ELSEIF iuo_LotCom.ExisteEncab(dw_loteEnca.Object.lofc_pltcod[li_fila], &
											dw_loteEnca.Object.lofc_espcod[li_fila], &
											dw_loteEnca.Object.lofc_lotefc[li_fila], False, SQLCA) THEN

		dw_loteEnca.SetItemStatus(li_fila, 0, Primary!, NotModified!)

	END IF
	
	IF NOT lb_retorno THEN EXIT
NEXT

FOR li_fila = 1 TO dw_lotedetatrans.RowCount()

	li_find	=	dw_loteencatrans.Find("lofc_pltcod = " + String(dw_lotedetatrans.Object.lofc_pltcod[li_fila]) + " AND " +&
												 "lofc_espcod = " + String(dw_lotedetatrans.Object.lofc_espcod[li_fila]) + " AND " +&
												 "lofc_lotefc = " + String(dw_lotedetatrans.Object.lofc_lotefc[li_fila]) , &
												 1, dw_loteencatrans.RowCount())
	IF li_find < 1 THEN
		MessageBox("Error", "El Lote " + String(dw_lotedetatrans.Object.lofc_pltcod[li_fila], '0000-')  + &
												   String(dw_lotedetatrans.Object.lofc_espcod[li_fila], '000-')   + &
												   String(dw_lotedetatrans.Object.lofc_lotefc[li_fila], '00000 ') + &
								  "no posee encabezado, no se podra generar recepción")
		lb_retorno = False
		
	ELSEIF NOT iuo_frio.ofp_recupera_periodofrio(sqlca, dw_lotedetatrans.Object.pefr_codigo[li_fila], True) THEN
		lb_retorno = False
		
//	ELSEIF NOT iuo_frio.tratamientofrio(sqlca, dw_lotedetatrans.Object.frio_tipofr[li_fila], True) THEN
//		lb_retorno = False
		
	ELSEIF iuo_LotCom.Existe(dw_loteDeta.Object.lofc_pltcod[li_fila], &
									 dw_loteDeta.Object.lofc_espcod[li_fila], &
									 dw_loteDeta.Object.lofc_lotefc[li_fila], &
									 dw_loteDeta.Object.lfcd_secuen[li_fila], False, SQLCA) THEN
		dw_loteDeta.SetItemStatus(li_fila, 0, Primary!, NotModified!)
	END IF
	
	IF NOT lb_retorno THEN EXIT
NEXT

IF lb_retorno THEN lb_retorno = ValidaMovto()

Return lb_retorno 
end function

public function boolean validamovto ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_movtoencatrans.RowCount()
	
	IF NOT iuo_camion.Existe(dw_movtoencatrans.Object.cami_clasifi[li_fila],&
									 dw_movtoencatrans.Object.cami_patent[li_fila], true, sqlca) THEN
		lb_retorno = False
//	ELSEIF NOT iuo_especie.Existe(dw_movtoencatrans.Object.espe_codigo[li_fila], true, sqlca) THEN
//		lb_retorno = False
	ELSEIF NOT iuo_planta.Existe(dw_movtoencatrans.Object.plde_codigo[li_fila], true, sqlca) THEN
		lb_retorno = False
//	ELSEIF NOT iuo_productor.Existe(dw_movtoencatrans.Object.prod_codigo[li_fila], true, sqlca) THEN
//		lb_retorno = False
	ELSEIF NOT iuo_cliente.Existe(dw_movtoencatrans.Object.clie_codigo[li_fila], true, sqlca) THEN
		lb_retorno = False
//	ELSEIF NOT iuo_motivo.Existe(dw_movtoencatrans.Object.moti_codigo[li_fila], true, sqlca) THEN
//		lb_retorno = False
	END IF
	
	IF NOT lb_retorno THEN EXIT
NEXT

FOR li_fila = 1 TO dw_movtodetatrans.RowCount()
	li_find	=	dw_loteencatrans.Find("lofc_pltcod = " + String(dw_movtodetatrans.Object.lofc_pltcod[li_fila]) + " AND " +&
												 "lofc_espcod = " + String(dw_movtodetatrans.Object.lofc_espcod[li_fila]) + " AND " +&
												 "lofc_lotefc = " + String(dw_movtodetatrans.Object.lofc_lotefc[li_fila]) , &
												  1, dw_loteencatrans.RowCount())
	IF li_find < 1 THEN
		MessageBox("Error", "El Lote " + String(dw_movtodetatrans.Object.lofc_pltcod[li_fila], '0000-')  + &
												   String(dw_movtodetatrans.Object.lofc_espcod[li_fila], '000-')   + &
												   String(dw_movtodetatrans.Object.lofc_lotefc[li_fila], '00000 ') + &
												   "no pertenece a este Movimiento, no se podra generar recepción")
		lb_retorno = False
	ELSEIF NOT iuo_camarasbode.Existe(dw_movtodetatrans.Object.plde_codigo[li_fila], &
												 dw_movtodetatrans.Object.cama_codigo[li_fila], True,sqlca) THEN
		lb_retorno = False
	END IF
NEXT

IF lb_retorno THEN lb_retorno = ValidaEnvases()

Return lb_retorno
end function

public function boolean validaenvases ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_envaencatrans.RowCount()
	
	IF NOT iuo_camion.Existe(dw_envaencatrans.Object.cami_clasifi[li_fila],&
									 dw_envaencatrans.Object.cami_patent[li_fila], true, sqlca) THEN
		lb_retorno = False
		
	ELSEIF NOT iuo_planta.Existe(dw_envaencatrans.Object.plde_codigo[li_fila], true, sqlca) THEN
		lb_retorno = False
		
	ELSEIF NOT iuo_productor.Existe(dw_envaencatrans.Object.prod_codigo[li_fila], true, sqlca) THEN
		lb_retorno = False
		
	ELSEIF NOT iuo_cliente.Existe(dw_envaencatrans.Object.clie_codigo[li_fila], true, sqlca) THEN
		lb_retorno = False
		
	ELSEIF NOT iuo_transp.Existe(dw_envaencatrans.Object.tran_codigo[li_fila], true, sqlca) THEN
		lb_retorno = False
		
	END IF
	
	IF NOT lb_retorno THEN EXIT
	
NEXT

FOR li_fila = 1 TO dw_envadetatrans.RowCount()
	IF NOT iuo_envases.Existe(dw_envadetatrans.Object.enva_tipoen[li_fila], &
									  dw_envadetatrans.Object.enva_codigo[li_fila], True, Sqlca) THEN
		lb_retorno = False
	END IF
	
	IF NOT lb_retorno THEN EXIT
NEXT

IF lb_retorno THEN lb_retorno = ValidaBins()

Return lb_retorno
end function

public subroutine enviamail ();String		ls_Nombre, 	ls_NomReporte, 	ls_Archivo, 	ls_DirectorioAct 
Long			ll_Fila, 	ll_consignatario, ll_Archivo
Boolean		lb_Existe
Integer		li_imprimio
str_parms	lstr_parms

SetPointer(HourGlass!)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", &
				 "Personal", RegString!, ls_DirectorioAct)

ls_archivo = String(ii_planta, '0000') + String(ii_cliente, '000') + String(ii_Movto, '0000000')

ls_NomReporte									=	'Archivos Planos Despacho ' + ls_archivo
lstr_parms.string_arg[1]					=	"1"
lstr_parms.string_arg[2]					=	ls_NomReporte
lstr_parms.string_arg[3]					=	"9"
lstr_parms.string_arg[30]					=	String(ii_planta)
ll_Archivo										= 	1
lstr_parms.string_arg[ll_Archivo+3]		=	ls_DirectorioAct + '\' + ls_archivo + "Indice_Comercial.txt"

FOR ll_Archivo = 1 TO 8
	lstr_parms.string_arg[ll_Archivo+4]	=	ls_DirectorioAct + '\' + dw_archivos.Object.Nombre[ll_archivo]
NEXT

OpenWithParm(w_correo_traspaso_interplanta, lstr_parms)

SetPointer(Arrow!)
end subroutine

public function boolean validabins ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_movtoBinsTrans.RowCount()
		IF NOT iuo_camarasbode.Existe(dw_movtoBinsTrans.Object.plde_codigo[li_fila], &
											dw_movtoBinsTrans.Object.cama_codigo[li_fila], True,sqlca) THEN
		lb_retorno = False
	END IF

	IF NOT lb_retorno THEN EXIT
NEXT

Return lb_retorno
end function

public function boolean wf_transamite ();Boolean		lb_Retorno = True
Integer		li_RC
Pipeline		lp_Create
DataStore	dw_errors

dw_errors	=	Create DataStore
lp_Create 	= 	Create p_pipe_wmeter

lp_Create.DataObject	=	'ppl_19_movtofrutacomenca'

IF wf_Errores(lp_Create.Start(sqlca, sqlTra, dw_errors, ii_cliente, ii_planta, ii_tipo, ii_movto), 'ppl_19_movtofrutacomenca') Then
	Destroy lp_Create
	lp_Create 				= 	Create p_pipe_wmeter
	lp_Create.DataObject = 	'ppl_22_movtofrutacomdeta'
	
	IF wf_Errores(lp_Create.Start(sqlca, sqlTra, dw_errors, ii_cliente, ii_planta, ii_tipo, ii_movto), 'ppl_22_movtofrutacomdeta') Then
		Destroy lp_Create
		lp_Create 				= 	Create p_pipe_wmeter
		lp_Create.DataObject = 	'ppl_spro_lotesfrutacomenca'
		
		IF wf_Errores(lp_Create.Start(sqlca, sqlTra, dw_errors, ii_cliente, ii_planta, ii_tipo, ii_movto), 'ppl_spro_lotesfrutacomenca') Then
			Destroy lp_Create
			lp_Create 				= 	Create p_pipe_wmeter
			lp_Create.DataObject = 	'ppl_spro_lotesfrutacomdeta'
			
			IF wf_Errores(lp_Create.Start(sqlca, sqlTra, dw_errors, ii_cliente, ii_planta, ii_tipo, ii_movto), 'ppl_spro_lotesfrutacomdeta') Then
				Destroy lp_Create
				lp_Create 				= 	Create p_pipe_wmeter
				lp_Create.DataObject = 	'ppl_8_movtoenvaenca'
				
				IF wf_Errores(lp_Create.Start(sqlca, sqlTra, dw_errors, ii_cliente, ii_planta, ii_tipo, ii_movto), 'ppl_8_movtoenvaenca') Then
					Destroy lp_Create
					lp_Create 				= 	Create p_pipe_wmeter
					lp_Create.DataObject = 	'ppl_9_movtoenvadeta'
					
					IF  wf_Errores(lp_Create.Start(sqlca, sqlTra, dw_errors, ii_cliente, ii_planta, ii_tipo, ii_movto), 'ppl_9_movtoenvadeta') Then
						Destroy lp_Create
						lp_Create 				= 	Create p_pipe_wmeter
						lp_Create.DataObject = 	'ppl_spro_movtobins'
						
						IF  wf_Errores(lp_Create.Start(sqlca, sqlTra, dw_errors, ii_cliente, ii_planta, ii_tipo, ii_movto), 'ppl_spro_movtobins') Then
							MessageBox('Atencion', 'Transmisión efectuada con Exito.', Information!, OK!)
						ELSE
							IF GeneraArchivos() THEN
								EnviaMail()
							END IF
							
						END IF
					ELSE
						IF GeneraArchivos() THEN
							EnviaMail()
						END IF
						
					END IF
				ELSE
					IF GeneraArchivos() THEN
						EnviaMail()
					END IF
					
				END IF
			ELSE
				IF GeneraArchivos() THEN
					EnviaMail()
				END IF
				
			END IF
		ELSE
			IF GeneraArchivos() THEN
				EnviaMail()
			END IF
			
		END IF
	ELSE
		IF GeneraArchivos() THEN
			EnviaMail()
		END IF
		
	END IF
ELSE
	IF GeneraArchivos() THEN
		EnviaMail()
	END IF
	
END IF

Destroy lp_Create
		
Return lb_Retorno
end function

public function boolean wf_errores (integer tipo, string titulo);Boolean lb_Retorno = True

Choose Case Tipo 
	Case  -1   
		MessageBox('Error en :' + Titulo, 'Apertura de PipeLine falló')
		lb_Retorno = False
		
	Case  -2      
		MessageBox('Error en :' + Titulo, 'Demasiadas Columnas')
		lb_Retorno = False
		
	Case  -3      
		MessageBox('Error en :' + Titulo, 'La tabla en cuestión ya existe')
		lb_Retorno = False
		
	Case  -4      
		MessageBox('Error en :' + Titulo, 'La tabla en cuestión no existe')
		lb_Retorno = False
		
	Case  -5      
		MessageBox('Error en :' + Titulo, 'Se ha perdido la conexion con el servidor')
		lb_Retorno = False
		
	Case  -6     
		MessageBox('Error en :' + Titulo, 'Existe un error uno de los argumentos del pipeline')
		lb_Retorno = False
		
	Case  -7      
		MessageBox('Error en :' + Titulo, 'Existen columnas con diferentes tipos de dato')
		lb_Retorno = False
		
	Case  -8      
		MessageBox('Error en :' + Titulo, 'Error Sql Fatal en Fuente')
		lb_Retorno = False
		
	Case  -9      
		MessageBox('Error en :' + Titulo, 'Error Sql Fatal en Destino')
		lb_Retorno = False
		
	Case -10     
		MessageBox('Error en :' + Titulo, 'Se ha excedido el Número Máximo de errores')
		lb_Retorno = False
		
	Case -12     
		MessageBox('Error en :' + Titulo, 'Sintaxís de tabla erronea')
		lb_Retorno = False
		
	Case -13     
		MessageBox('Error en :' + Titulo, 'Llave primaria requerida, pero no suministrada')
		lb_Retorno = False
		
	Case -15     
		MessageBox('Error en :' + Titulo, 'El Pipeline ya se encuentra en ejecucion')
		lb_Retorno = False
		
	Case -16     
		MessageBox('Error en :' + Titulo, 'Error en Base de Datos fuente')
		lb_Retorno = False
		
	Case -17     
		MessageBox('Error en :' + Titulo, 'Error en Base de Datos destino')
		lb_Retorno = False
		
	Case -18     
		MessageBox('Error en :' + Titulo, 'Base de datos destino solo lectura')
		lb_Retorno = False
		
End Choose

Return lb_retorno
end function

public function boolean wf_validamovto (integer ai_tipo, long al_numero, transaction at_transaccion);Boolean	lb_Retorno = True
Long		ll_Cantidad

SELECT IsNull(mfco_numero, 0)
  INTO :ll_Cantidad
  FROM dbo.spro_movtofrutacomenca
 WHERE mfco_tipdoc 	=	:ai_tipo
	AND mfco_docrel	=	:al_numero
 USING at_transaccion;

IF at_transaccion.SqlCode = -1 THEN
	lb_Retorno = False
ELSE
	IF ll_Cantidad > 0 THEN
		MessageBox('Error', 'Este movimiento ya fue generado con el Numero Movimiento: ' + String(ll_Cantidad, '00000000') + &
			' Recepcion de Plantas Propias.' , Information!, OK!)
		lb_Retorno = FALSE
	END IF
END IF

RETURN lb_Retorno
end function

event constructor;iuo_categoria							=	Create uo_categorias
iuo_frio									=	Create uo_periodofrio	
iuo_variedad							=	Create uo_variedades
iuo_envases								=	Create uo_envases
iuo_camion								=	Create uo_camiones
iuo_especie								=	Create uo_especie
iuo_planta								=	Create uo_plantadesp
iuo_productor							=	Create uo_productores
iuo_cliente								=	Create uo_clientesprod
iuo_motivo								=	Create uo_motivodespacho
iuo_transp								=	Create uo_transportista
iuo_bins									=	Create uo_bins
iuo_camarasbode						=	Create uo_camarasbode
iuo_Lotes								=	Create uo_spro_lotesfrutacomenc
iuo_LotCom								=	Create uo_lotesfrutacomer

dw_archivos								=	Create DataStore
dw_envaencatrans						=	Create DataStore
dw_loteencatrans						=	Create DataStore
dw_movtoencatrans						=	Create DataStore
dw_movtodetatrans						=	Create DataStore
dw_lotedetatrans						=	Create DataStore
dw_envadetatrans						=	Create DataStore
dw_envadeta								=	Create DataStore
dw_lotedeta								=	Create DataStore
dw_movtodeta							=	Create DataStore
dw_movtoenca							=	Create DataStore
dw_loteenca								=	Create DataStore
dw_envaenca								=	Create DataStore
dw_movtobins							=	Create DataStore
dw_movtobinsTrans 					= 	Create DataStore
dw_binstrans							= 	Create DataStore
dw_bins									= 	Create DataStore

dw_movtoencaTrans.dataobject		= 	"dw_mant_movtofrutacomenca_movtos_trans"
dw_movtodetaTrans.dataobject 		= 	"dw_mues_movtofrutacomdeta_ventas_trans"
dw_envaencaTrans.dataobject 		= 	"dw_mant_movtoenvaenca_trans"
dw_envadetaTrans.dataobject 		= 	"dw_mues_movtoenvadeta_trans"
dw_loteencaTrans.dataobject 		= 	"dw_gene_spro_lotesfrutacomenc_trans"
dw_lotedetaTrans.dataobject		= 	"dw_gene_spro_lotesfrutacomdeta_trans"
dw_movtobinsTrans.dataobject		= 	"dw_spro_movtobins_trans"
dw_binsTrans.dataobject				= 	"dw_mues_spro_bins_trans"

dw_archivos.dataobject 				= 	"dw_archivos"

dw_movtoenca.dataobject 			= 	"dw_mant_movtofrutacomenca_movtos_origen"
dw_movtodeta.dataobject 			= 	"dw_mues_movtofrutacomdeta_ventas_origen"
dw_envaenca.dataobject 				= 	"dw_mant_movtoenvaenca_origen"
dw_envadeta.dataobject 				= 	"dw_mues_movtoenvadeta_origen"
dw_loteenca.dataobject 				= 	"dw_gene_spro_lotesfrutacomenc_origen"
dw_lotedeta.dataobject 				= 	"dw_gene_spro_lotesfrutacomdeta_origen"
dw_movtobins.dataobject 			= 	"dw_spro_movtobins_origen"
dw_bins.dataobject 					=	"dw_mues_spro_bins_origen"
end event

on nvuo_traspaso_interplanta_comercial.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nvuo_traspaso_interplanta_comercial.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event destructor;Destroy iuo_categoria
Destroy iuo_frio
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

Destroy dw_archivos	
Destroy dw_envaencatrans
Destroy dw_loteencatrans
Destroy dw_movtoencatrans
Destroy dw_movtodetatrans
Destroy dw_lotedetatrans
Destroy dw_envadetatrans
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

Destroy SqlTra

garbageCollect()
end event

