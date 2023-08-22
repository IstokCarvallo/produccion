$PBExportHeader$uo_traspaso_interplanta.sru
forward
global type uo_traspaso_interplanta from userobject
end type
type dw_archivos from datawindow within uo_traspaso_interplanta
end type
type dw_envaencatrans from datawindow within uo_traspaso_interplanta
end type
type dw_loteencatrans from datawindow within uo_traspaso_interplanta
end type
type dw_movtodetatrans from datawindow within uo_traspaso_interplanta
end type
type dw_lotedetatrans from datawindow within uo_traspaso_interplanta
end type
type dw_envadetatrans from datawindow within uo_traspaso_interplanta
end type
type dw_granpesatrans from datawindow within uo_traspaso_interplanta
end type
type dw_granpesa from datawindow within uo_traspaso_interplanta
end type
type dw_envadeta from datawindow within uo_traspaso_interplanta
end type
type dw_lotedeta from datawindow within uo_traspaso_interplanta
end type
type dw_movtodeta from datawindow within uo_traspaso_interplanta
end type
type cb_1 from commandbutton within uo_traspaso_interplanta
end type
type dw_loteenca from datawindow within uo_traspaso_interplanta
end type
type dw_envaenca from datawindow within uo_traspaso_interplanta
end type
type dw_movtoencatrans from datawindow within uo_traspaso_interplanta
end type
type dw_movtoenca from datawindow within uo_traspaso_interplanta
end type
end forward

global type uo_traspaso_interplanta from userobject
integer width = 2121
integer height = 1920
long backcolor = 12632256
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event traspasadatos ( )
event ue_recuperadatos ( )
event ue_guardar ( )
event ue_antesguardar ( )
dw_archivos dw_archivos
dw_envaencatrans dw_envaencatrans
dw_loteencatrans dw_loteencatrans
dw_movtodetatrans dw_movtodetatrans
dw_lotedetatrans dw_lotedetatrans
dw_envadetatrans dw_envadetatrans
dw_granpesatrans dw_granpesatrans
dw_granpesa dw_granpesa
dw_envadeta dw_envadeta
dw_lotedeta dw_lotedeta
dw_movtodeta dw_movtodeta
cb_1 cb_1
dw_loteenca dw_loteenca
dw_envaenca dw_envaenca
dw_movtoencatrans dw_movtoencatrans
dw_movtoenca dw_movtoenca
end type
global uo_traspaso_interplanta uo_traspaso_interplanta

type variables
Transaction			SQLTRA, sqlca_1
Boolean				ib_Conectado

Integer				ii_Planta, ii_Tipo, ii_Movto, ii_Cliente, ii_sentido, ii_ManAut, retorno, ii_plantades
Window				w_parent			

uo_categorias		iuo_categoria
uo_periodofrio		iuo_frio
uo_condicioncc		iuo_condicion
uo_variedades		iuo_variedad
uo_envases			iuo_envases
uo_camiones			iuo_camion
uo_especie			iuo_especie
uo_plantadesp		iuo_planta
uo_productores		iuo_productor
uo_Clientesprod		iuo_cliente
uo_motivodespacho	iuo_motivo
uo_transportista	iuo_transp
uo_bins				iuo_bins
uo_camarasfrigo	iuo_camarasbode
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
end prototypes

event traspasadatos();//Datos de prueba

sqlca = sqlca_1

dw_movtoEnca.SetTransObject(sqlca)
dw_movtoDeta.SetTransObject(sqlca)
dw_loteEnca.SetTransObject(sqlca)
dw_loteDeta.SetTransObject(sqlca)
dw_envaEnca.SetTransObject(sqlca)
dw_envaDeta.SetTransObject(sqlca)
dw_granpesa.SetTransObject(sqlca)


dw_movtoencatrans.dataobject 	= "dw_mant_movtofrutagranenca_trans"
dw_movtodetatrans.dataobject 	= "dw_mues_movtofrutagraneldeta_trans"
dw_envaencatrans.dataobject 	= "dw_mant_movtoenvaenca_trans"
dw_envadetatrans.dataobject 	= "dw_mues_movtoenvadeta_trans"
dw_loteencatrans.dataobject 	= "dw_mues_spro_lotesfrutagranel_trans"
dw_lotedetatrans.dataobject 	= "dw_mues_spro_lotesfrutagradet_trans"
dw_granpesatrans.dataobject 	= "dw_pesaje_romana_trans"
//dw_movtobinstrans.dataobject 	= "dw_spro_movtobins_trans"

dw_archivos.dataobject 			= "dw_archivos"

dw_movtoenca.dataobject 		= "dw_mant_movtofrutagranenca_origen"
dw_movtodeta.dataobject 		= "dw_mues_movtofrutagraneldeta_origen"
dw_granpesa.dataobject 			= "dw_pesaje_romana_origen"
dw_envaenca.dataobject 			= "dw_mant_movtoenvaenca_origen"
dw_envadeta.dataobject 			= "dw_mues_movtoenvadeta_origen"
dw_loteenca.dataobject 			= "dw_mues_spro_lotesfrutagranel_origen"
dw_lotedeta.dataobject 			= "dw_mues_spro_lotesfrutagradet_origen"





dw_movtoEnca.SetTransObject(sqlca)
dw_movtoDeta.SetTransObject(sqlca)
dw_loteEnca.SetTransObject(sqlca)
dw_loteDeta.SetTransObject(sqlca)
dw_envaEnca.SetTransObject(sqlca)
dw_envaDeta.SetTransObject(sqlca)
dw_granpesa.SetTransObject(sqlca)
//dw_movtobins.SetTransObject(sqlca)

//ii_planta 	= 	1
//ii_tipo		=	22
//ii_movto		=	626
//ii_cliente	=	81

//ii_sentido 	= 	1
//ii_ManAut	=	1

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
		This.TriggerEvent("ue_RecuperaDatos")
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


IF ii_sentido = 1 THEN
	DO
		IF dw_MovtoEnca.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_MovtoDeta.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1	OR &
			dw_LoteEnca.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_LoteDeta.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_EnvaEnca.Retrieve(ii_planta, ii_tipo, ii_movto, 1, ii_cliente) = -1 OR &
			dw_EnvaDeta.Retrieve(ii_planta, ii_tipo, ii_movto, 1, ii_cliente) = -1 OR &
			dw_granPesa.Retrieve(ii_planta, ii_tipo, ii_movto) = -1 /*OR &
			dw_movtobins.Retrieve(ii_planta, ii_movto, ii_cliente) = -1 */THEN
			
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
		END IF
	LOOP WHILE respuesta = 1
ELSE
	dw_movtoEncaTrans.SetTransObject(sqlca)
	dw_movtoDetaTrans.SetTransObject(sqlca)
	dw_loteEncaTrans.SetTransObject(sqlca)
	dw_loteDetaTrans.SetTransObject(sqlca)
	dw_envaEncaTrans.SetTransObject(sqlca)
	dw_envaDetaTrans.SetTransObject(sqlca)
	dw_granpesaTrans.SetTransObject(sqlca)
	//dw_movtoBinsTrans.SetTransObject(sqlca)
	
	DO
		IF dw_MovtoEncaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_MovtoDetaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1	OR &
			dw_LoteEncaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_LoteDetaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, ii_cliente) = -1 OR &
			dw_EnvaEncaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, 1, ii_cliente) = -1 OR &
			dw_EnvaDetaTrans.Retrieve(ii_planta, ii_tipo, ii_movto, 1, ii_cliente) = -1 OR &
			dw_granPesaTrans.Retrieve(ii_planta, ii_tipo, ii_movto) = -1 /*OR &
			dw_movtoBinsTrans.Retrieve(ii_planta, ii_movto, ii_cliente) = -1*/ THEN
			
			respuesta 	= 	MessageBox("Error en Base de Datos", &
											  "No es posible conectar la Base de Datos.", &
											  Information!, RetryCancel!)
			Message.DoubleParm = -1
		END IF
	LOOP WHILE respuesta = 1
END IF
end event

event ue_guardar();Integer	li_fila, li_estado
Boolean 	lb_retorno = True
SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

IF ConectaTrans(ii_plantades) THEN 
	w_main.SetMicroHelp("Comenzando Proceso Automatico.")	
	FOR li_fila = 1 TO dw_movtoEnca.RowCount()
		li_estado = dw_movtoEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_movtoDeta.RowCount()
		li_estado = dw_movtoDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_loteEnca.RowCount()
		li_estado = dw_loteEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_loteDeta.RowCount()
		li_estado = dw_loteDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_envaEnca.RowCount()
		li_estado = dw_envaEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_envaDeta.RowCount()
		li_estado = dw_envaDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
//	FOR li_fila = 1 TO dw_movtoBins.RowCount()
//		li_estado = dw_movtoBins.SetItemStatus(li_fila, 0, Primary!, NewModified!)
//		IF li_estado = -1 THEN lb_retorno = False
//	NEXT
	FOR li_fila = 1 TO dw_granpesa.RowCount()
		li_estado = dw_granpesa.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	
	IF wf_actualiza_db(False) THEN
		w_main.SetMicroHelp("Información Grabada.")	
	ELSE
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN
	END IF
ELSE
	w_main.SetMicroHelp("Comenzando Proceso Via E-Mail")	
	IF GeneraArchivos() THEN
		EnviaMail()
	END IF
END IF
	
end event

public function boolean cargadatos ();Integer	li_fila, li_estado
Boolean 	lb_retorno = True

IF dw_movtoEnca.RowsCopy(1, dw_movtoEnca.RowCount(), &
								  Primary!, dw_MovtoEncaTrans, 1, Primary!) = -1 OR &
	dw_movtoDeta.RowsCopy(1, dw_movtoDeta.RowCount(), &
								  Primary!, dw_movtoDetaTrans, 1, Primary!) = -1 OR &
	dw_loteEnca.RowsCopy(1, dw_loteEnca.RowCount(), &
								  Primary!, dw_loteEncaTrans, 1, Primary!) = -1 OR &
	dw_loteDeta.RowsCopy(1, dw_loteDeta.RowCount(), &
								  Primary!, dw_loteDetaTrans, 1, Primary!) = -1 OR &
	dw_envaEnca.RowsCopy(1, dw_envaEnca.RowCount(), &
								  Primary!, dw_envaEncaTrans, 1, Primary!) = -1 OR &
	dw_envaDeta.RowsCopy(1, dw_envaDeta.RowCount(), &
								  Primary!, dw_envaDetaTrans, 1, Primary!) = -1 OR &
/*	dw_movtobins.RowsCopy(1, dw_movtobins.RowCount(), &
								  Primary!, dw_movtobinsTrans, 1, Primary!) = -1 OR */&
	dw_granpesa.RowsCopy(1, dw_granpesa.RowCount(), &
								  Primary!, dw_granpesaTrans, 1, Primary!) = -1 THEN
	lb_retorno = False
ELSE
	FOR li_fila = 1 TO dw_movtoEncaTrans.RowCount()
		li_estado = dw_movtoEncaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_movtoDetaTrans.RowCount()
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
		li_estado = dw_envaEncaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_envaDetaTrans.RowCount()
		li_estado = dw_envaDetaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
//	FOR li_fila = 1 TO dw_movtobinsTrans.RowCount()
//		li_estado = dw_movtobinsTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
//		IF li_estado = -1 THEN lb_retorno = False
//	NEXT
	FOR li_fila = 1 TO dw_granpesaTrans.RowCount()
		li_estado = dw_granpesaTrans.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
END IF

Return lb_retorno
end function

public function boolean conectatrans (integer ai_planta);String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ii_sentido = 1 THEN
	
	IF ib_Conectado THEN
		DISCONNECT USING SQLTRA;
	ELSE
		SQLTRA	=	Create Transaction
	END IF
	
	ls_Usuario				=	sqlca.UserId
	ls_Password				=	sqlca.DbPass
	
	SELECT cone_nomodb,cone_nomser,cone_nombas,
			cone_nodbms,cone_nomusu,cone_passwo  
	 INTO :ls_nomodb,:ls_nomser,:ls_nombas,
			:ls_nodbms,:ls_Usuario,:ls_Password
	 FROM dba.prodconectividad   
	WHERE cone_codigo = :ai_planta;
	
	SQLTRA.ServerName	=	ls_nomser
	SQLTRA.DataBase	=	ls_nombas
	SQLTRA.Dbms			= 	ls_nodbms
	SQLTRA.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
									";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
	CONNECT USING SQLTRA;
	
	IF SQLTRA.SQLCode = -1 THEN
		MessageBox("Error","Imposible conectar a Planta Destino, se enviaran Archivos Planos")
	ELSE
		ib_Conectado	=	True
		dw_movtoEncaTrans.SetTransObject(SQLTRA)
		dw_movtoDetaTrans.SetTransObject(SQLTRA)
		dw_loteEncaTrans.SetTransObject(SQLTRA)
		dw_loteDetaTrans.SetTransObject(SQLTRA)
		dw_envaEncaTrans.SetTransObject(SQLTRA)
		dw_envaDetaTrans.SetTransObject(SQLTRA)
		dw_granpesaTrans.SetTransObject(SQLTRA)
		dw_granpesaTrans.SetTransObject(SQLTRA)
	END IF
	
ELSE
	ib_Conectado	=	True
END IF

Return ib_conectado
end function

public function boolean wf_actualiza_db (boolean borrando);Boolean lb_retorno


IF Borrando THEN
	IF dw_movtoDetaTrans.Update(True, False) = 1 THEN 						   //Detalle
		IF dw_loteDetaTrans.Update(True, False) = 1 THEN					   //Detalle de Lotes
			IF dw_granpesaTrans.Update(True, False) = 1 THEN					//Movimiento de Bins
				IF dw_loteEncaTrans.Update(True, False) = 1 THEN				//Lotes
					IF dw_granpesaTrans.Update(True, False) = 1 THEN			//Detalle de Pesaje
						IF dw_movtoEncaTrans.Update(True, False) = 1 THEN		//Encabezado
							IF dw_envaDetaTrans.Update(True,False) = 1 THEN		//Envases Recepcionados
								IF dw_envaEncaTrans.Update(True,False) = 1 THEN	//Encabezados Envases
									Commit;
								
									IF sqltra.SQLCode <> 0 THEN
										F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
										RollBack;
									ELSE
										lb_Retorno	=	True
										dw_movtoEncaTrans.ResetUpdate()
										dw_movtoDetaTrans.ResetUpdate()
										dw_loteEncaTrans.ResetUpdate()
										dw_loteDetaTrans.ResetUpdate()
										dw_envaEncaTrans.ResetUpdate()
										dw_envaDetaTrans.ResetUpdate()
										dw_granpesaTrans.ResetUpdate()
										dw_granpesaTrans.ResetUpdate()
									END IF
								ELSE
									F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
									RollBack;
								END IF
							ELSE
								F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
								RollBack;
							END IF
						ELSE
							F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
							RollBack;
						END IF
					ELSE
						F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
	END IF
ELSE
	IF dw_movtoEncaTrans.Update(True, False) = 1 THEN		 				//Encabezado
		IF dw_loteEncaTrans.Update(True, False) = 1 THEN					//Lotes
			IF dw_loteDetaTrans.Update(True, False) = 1 THEN				//Detalle de Lotes
				IF dw_movtoDetaTrans.Update(True, False) = 1 THEN			//Detalle
					IF dw_envaDetaTrans.Update(True,False) = 1 THEN			//Envases Recepcionados
						IF dw_envaEncaTrans.Update(True,False) = 1 THEN		//Envases Encabezado
							IF dw_granpesaTrans.Update(True,False) = 1 THEN	//Detalle Pesaje
								IF dw_granpesaTrans.Update(True,False) = 1 THEN	//Movimiento Bins
									Commit;
									IF sqltra.SQLCode <> 0 THEN
										F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
									ELSE
										lb_Retorno	=	True
										
										dw_movtoEncaTrans.ResetUpdate()
										dw_movtoDetaTrans.ResetUpdate()
										dw_loteEncaTrans.ResetUpdate()
										dw_loteDetaTrans.ResetUpdate()
										dw_envaEncaTrans.ResetUpdate()
										dw_envaDetaTrans.ResetUpdate()
										dw_granpesaTrans.ResetUpdate()
									END IF
								ELSE
									F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
									RollBack;
								END IF
							ELSE
								F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
								RollBack;
							END IF
						ELSE
							F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
							RollBack;
						END IF
					ELSE
						F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqltra, "Despacho InterPlanta")
		RollBack;
	END IF
END IF

Return lb_retorno
end function

public function boolean generaarchivos ();Boolean	lb_retorno = True
String	ls_ruta, ls_archivo
Integer	ll_fila


RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

ls_archivo = String(ii_planta, '0000') + String(ii_cliente, '000') + String(ii_Movto, '0000000') 

dw_archivos.InsertRow(0)

ll_fila = dw_movtoEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MovEnc.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[1] = ls_archivo + "MovEnc.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_movtoDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MovDet.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[2] = ls_archivo + "MovDet.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_loteEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "LotEnc.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[3] = ls_archivo + "LotEnc.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_loteDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "LotDet.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[4] = ls_archivo + "LotDet.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_envaEncaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "EnvEnc.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[5] = ls_archivo + "EnvEnc.txt"
	dw_archivos.InsertRow(0)
END IF

ll_fila = dw_envaDetaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "EnvDet.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[6] = ls_archivo + "EnvDet.txt"
	dw_archivos.InsertRow(0)
END IF

//ll_fila = dw_movtobinsTrans.SaveAs(ls_ruta + "\" + ls_archivo + "MvtBin.txt", Text!, False)
//IF ll_fila = 1 THEN 
//	dw_archivos.Object.Nombre[7] = ls_archivo + "MvtBin.txt"
//	dw_archivos.InsertRow(0)
//END IF
//
ll_fila = dw_granpesaTrans.SaveAs(ls_ruta + "\" + ls_archivo + "GrnPes.txt", Text!, False)
IF ll_fila = 1 THEN 
	dw_archivos.Object.Nombre[8] = ls_archivo + "GrnPes.txt"
END IF

IF dw_archivos.RowCount() = 8 THEN
	dw_archivos.SaveAs(ls_ruta + "\" + ls_archivo + "Indice.txt", Text!, False)
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

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

li_ret	= 	GetFileOpenName("Seleccione Archivo", ls_ruta , ls_archivo, "txt", &
								  + "Archivos Indice (*Indice.txt),*Indice.txt", ls_ruta, 18)

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
	//00010810000626Indice.txt
	ii_planta	=	Integer(Left(ls_archivo, 4)	)
	ii_cliente	=	Integer( Mid(ls_archivo, 5, 3))
	ii_movto		=	Integer( Mid(ls_archivo, 8, 7))
	
	ls_archivo 	= String(ii_planta, '0000') + String(ii_cliente, '000') + String(ii_Movto, '0000000')
	
	li_ret =	dw_archivos.ImportFile(ls_ruta + "\" + ls_archivo + "Indice.txt")
	
	IF li_ret < 1 THEN//Error con el archivo indice
		MessageBox("Error", "El Archivo " + ls_archivo + "Indice.txt" + " no Existe o no posee datos Validos")
	ELSE
		
		IF dw_archivos.RowCount() <> 8 THEN
			//Valida que traiga los 7 nombres de archivos
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
					CASE ls_archivo + "MovEnc.txt"
						li_ret = dw_movtoEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Encabezado del Movimiento")
							lb_retorno = False
							Exit
						END IF
						
					CASE ls_archivo + "MovDet.txt"
						li_ret = dw_movtoDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Detalle del Movimiento")
							lb_retorno = False
							Exit
						END IF
						
					CASE ls_archivo + "LotEnc.txt"
						li_ret = dw_loteEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Encabezado de los Lotes")
							lb_retorno = False
							Exit
						END IF
						
					CASE ls_archivo + "LotDet.txt"
						li_ret = dw_loteDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Detalle de los Lotes")
							lb_retorno = False
							Exit
						END IF
						
					CASE ls_archivo + "EnvEnc.txt"
						li_ret = dw_envaEncaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Encabezado de Envases")
							lb_retorno = False
							Exit
						END IF
						
					CASE ls_archivo + "EnvDet.txt"
						li_ret = dw_envaDetaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Detalle de Envases")
							lb_retorno = False
							Exit
						END IF
						
				/*	CASE ls_archivo + "MvtBin.txt"
						li_ret = dw_movtoBinsTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Movimientos de Bins/Base Pallets")
							lb_retorno = False
							Exit
						END IF	*/
						
					CASE ls_archivo + "GrnPes.txt"
						li_ret = dw_granpesaTrans.ImportFile(ls_ruta + "\" + dw_archivos.Object.Nombre[li_fila])
						IF li_ret < 1 THEN
							MessageBox("Error", "No se pudo Cargar Detalle de Pesajes")
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

IF dw_movtoEncaTrans.RowsCopy(1, dw_movtoEncaTrans.RowCount(), &
							  Primary!, dw_MovtoEnca, 1, Primary!) = -1 OR &
	dw_movtoDetaTrans.RowsCopy(1, dw_movtoDetaTrans.RowCount(), &
							  Primary!, dw_movtoDeta, 1, Primary!) = -1 OR &
	dw_loteEncaTrans.RowsCopy(1,  dw_loteEncaTrans.RowCount(), &
							  Primary!, dw_loteEnca, 1, Primary!) = -1 OR &
	dw_loteDetaTrans.RowsCopy(1,  dw_loteDetaTrans.RowCount(), &
							  Primary!, dw_loteDeta, 1, Primary!) = -1 OR &
	dw_envaEncaTrans.RowsCopy(1,  dw_envaEncaTrans.RowCount(), &
							  Primary!, dw_envaEnca, 1, Primary!) = -1 OR &
	dw_envaDetaTrans.RowsCopy(1,  dw_envaDetaTrans.RowCount(), &
							  Primary!, dw_envaDeta, 1, Primary!) = -1 OR &
/*	dw_movtoBinsTrans.RowsCopy(1, dw_movtoBinsTrans.RowCount(), &
							  Primary!, dw_movtoBins, 1, Primary!) = -1 OR*/ &
	dw_granpesaTrans.RowsCopy(1,  dw_granpesaTrans.RowCount(), &
							  Primary!, dw_granpesa, 1, Primary!) = -1 THEN
	lb_retorno = False
ELSE
	FOR li_fila = 1 TO dw_movtoEnca.RowCount()
		li_estado = dw_movtoEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_movtoDeta.RowCount()
		li_estado = dw_movtoDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_loteEnca.RowCount()
		li_estado = dw_loteEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_loteDeta.RowCount()
		li_estado = dw_loteDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_envaEnca.RowCount()
		li_estado = dw_envaEnca.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
	FOR li_fila = 1 TO dw_envaDeta.RowCount()
		li_estado = dw_envaDeta.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
//	FOR li_fila = 1 TO dw_movtoBins.RowCount()
//		li_estado = dw_movtoBins.SetItemStatus(li_fila, 0, Primary!, NewModified!)
//		IF li_estado = -1 THEN lb_retorno = False
//	NEXT
	FOR li_fila = 1 TO dw_granpesa.RowCount()
		li_estado = dw_granpesa.SetItemStatus(li_fila, 0, Primary!, NewModified!)
		IF li_estado = -1 THEN lb_retorno = False
	NEXT
END IF

IF lb_retorno THEN lb_Retorno = ValidaLotes()

Return lb_retorno
end function

public function boolean validalotes ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

FOR li_fila = 1 TO dw_loteencatrans.RowCount()
	
	li_find	=	dw_movtodetatrans.Find("lote_pltcod = " + String(dw_lotedetatrans.Object.lote_pltcod[li_fila]) + " AND " +&
												  "lote_espcod = " + String(dw_lotedetatrans.Object.lote_espcod[li_fila]) + " AND " +&
												  "lote_codigo = " + String(dw_lotedetatrans.Object.lote_codigo[li_fila]) , &
												  1, dw_loteencatrans.RowCount())
	IF li_find < 1 THEN
		MessageBox("Error", 	"El Lote " + String(dw_lotedetatrans.Object.lote_pltcod[li_fila], '0000-')  + &
												   String(dw_lotedetatrans.Object.lote_espcod[li_fila], '000-')   + &
												   String(dw_lotedetatrans.Object.lote_codigo[li_fila], '00000 ') + &
									"no pertenece a este Movimiento, no se podra generar recepción")
		lb_retorno = False
	ELSEIF NOT iuo_categoria.Existe(dw_loteencatrans.Object.cate_codigo[li_fila], True, sqlca) THEN
		lb_retorno = False
	ELSEIF NOT iuo_frio.ofp_recupera_periodofrio(sqlca, dw_loteencatrans.Object.pefr_codigo[li_fila], True) THEN
		lb_retorno = False
	ELSEIF NOT iuo_condicion.Existe(dw_loteencatrans.Object.cocc_codigo[li_fila], True, sqlca) THEN
		lb_retorno = False
	ELSEIF NOT iuo_especie.Existe(dw_loteencatrans.Object.lote_espcod[li_fila], True, sqlca) THEN
		lb_retorno = False
	ELSEIF NOT iuo_variedad.Existe(dw_loteencatrans.Object.lote_espcod[li_fila], &
									dw_loteencatrans.Object.vari_codigo[li_fila], True, sqlca) THEN
		lb_retorno = False
	ELSE// si el lote existe, se debe agregar los nuevos bultos a los ya existentes.
		
	END IF
	
	IF NOT lb_retorno THEN EXIT
NEXT

FOR li_fila = 1 TO dw_lotedetatrans.RowCount()
	
	li_find	=	dw_loteencatrans.Find("lote_pltcod = " + String(dw_lotedetatrans.Object.lote_pltcod[li_fila]) + " AND " +&
												 "lote_espcod = " + String(dw_lotedetatrans.Object.lote_espcod[li_fila]) + " AND " +&
												 "lote_codigo = " + String(dw_lotedetatrans.Object.lote_codigo[li_fila]) , &
												 1, dw_loteencatrans.RowCount())
	IF li_find < 1 THEN
		MessageBox("Error", "El Lote " + String(dw_lotedetatrans.Object.lote_pltcod[li_fila], '0000-')  + &
												   String(dw_lotedetatrans.Object.lote_espcod[li_fila], '000-')   + &
												   String(dw_lotedetatrans.Object.lote_codigo[li_fila], '00000 ') + &
								  "no posee encabezado disponible, no se podra generar recepción")
		lb_retorno = False
	ELSEIF NOT iuo_envases.Existe(dw_lotedetatrans.Object.enva_tipoen[li_fila], &
									      dw_lotedetatrans.Object.enva_codigo[li_fila], True, Sqlca) THEN
		lb_retorno = False
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
	ELSEIF NOT iuo_especie.Existe(dw_movtoencatrans.Object.espe_codigo[li_fila], true, sqlca) THEN
		lb_retorno = False
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
	li_find	=	dw_loteencatrans.Find("lote_pltcod = " + String(dw_movtodetatrans.Object.lote_pltcod[li_fila]) + " AND " +&
												 "lote_espcod = " + String(dw_movtodetatrans.Object.lote_espcod[li_fila]) + " AND " +&
												 "lote_codigo = " + String(dw_movtodetatrans.Object.lote_codigo[li_fila]) , &
												  1, dw_loteencatrans.RowCount())
	IF li_find < 1 THEN
		MessageBox("Error", "El Lote " + String(dw_movtodetatrans.Object.lote_pltcod[li_fila], '0000-')  + &
												   String(dw_movtodetatrans.Object.lote_espcod[li_fila], '000-')   + &
												   String(dw_movtodetatrans.Object.lote_codigo[li_fila], '00000 ') + &
												   "no pertenece a este Movimiento, no se podra generar recepción")
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
//	ELSEIF NOT iuo_motivo.Existe(dw_envaencatrans.Object.moti_codigo[li_fila], true, sqlca) THEN
//		lb_retorno = False
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

Return lb_retorno
end function

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
lstr_parms.string_arg[3]					=	String(9)
lstr_parms.string_arg[30]					=	String(ii_planta)

ll_Archivo = 1

lstr_parms.string_arg[ll_Archivo+3]		=	ls_DirectorioAct + '\' + ls_archivo + "Indice.txt"

FOR ll_Archivo = 1 TO 8
	lstr_parms.string_arg[ll_Archivo+4]	=	ls_DirectorioAct + '\' + dw_archivos.Object.Nombre[ll_archivo]
NEXT

OpenWithParm(w_correo_traspaso_interplanta, lstr_parms)

SetPointer(Arrow!)
end subroutine

public function boolean validabins ();Boolean 	lb_retorno = True
Integer	li_fila, li_find

//FOR li_fila = 1 TO dw_movtoBinsTrans.RowCount()
//	
//	li_find	=	dw_movtodetatrans.Find("lote_pltcod = " + String(dw_movtoBinsTrans.Object.plde_codigo[li_fila]) + " AND " +&
//												  "lote_espcod = " + String(dw_movtoBinsTrans.Object.lote_espcod[li_fila]) + " AND " +&
//												  "lote_codigo = " + String(dw_movtoBinsTrans.Object.lote_codigo[li_fila]) , &
//												  1, dw_loteencatrans.RowCount())
//	IF li_find < 1 THEN
//		MessageBox("Error", 	"El Lote " + String(dw_movtoBinsTrans.Object.plde_codigo[li_fila], '0000-')  + &
//												   String(dw_movtoBinsTrans.Object.lote_espcod[li_fila], '000-')   + &
//												   String(dw_movtoBinsTrans.Object.lote_codigo[li_fila], '00000 ') + &
//									"no pertenece a este Movimiento, no se podra generar recepción")
//		lb_retorno = False
//	ELSEIF NOT iuo_bins.Existe(dw_movtoBinsTrans.Object.clie_codigo[li_fila],&
//										dw_movtoBinsTrans.Object.plde_codigo[li_fila],&
//										dw_movtoBinsTrans.Object.bins_numero[li_fila], sqlca, True) THEN
//		lb_retorno = False
//	ELSEIF NOT iuo_camarasbode.Existe(dw_movtoBinsTrans.Object.plde_codigo[li_fila], &
//												 dw_movtoBinsTrans.Object.cama_codigo[li_fila], True,sqlca) THEN
//		lb_retorno = False
//	END IF
//	
//	IF NOT lb_retorno THEN EXIT
//NEXT

Return lb_retorno
end function

on uo_traspaso_interplanta.create
this.dw_archivos=create dw_archivos
this.dw_envaencatrans=create dw_envaencatrans
this.dw_loteencatrans=create dw_loteencatrans
this.dw_movtodetatrans=create dw_movtodetatrans
this.dw_lotedetatrans=create dw_lotedetatrans
this.dw_envadetatrans=create dw_envadetatrans
this.dw_granpesatrans=create dw_granpesatrans
this.dw_granpesa=create dw_granpesa
this.dw_envadeta=create dw_envadeta
this.dw_lotedeta=create dw_lotedeta
this.dw_movtodeta=create dw_movtodeta
this.cb_1=create cb_1
this.dw_loteenca=create dw_loteenca
this.dw_envaenca=create dw_envaenca
this.dw_movtoencatrans=create dw_movtoencatrans
this.dw_movtoenca=create dw_movtoenca
this.Control[]={this.dw_archivos,&
this.dw_envaencatrans,&
this.dw_loteencatrans,&
this.dw_movtodetatrans,&
this.dw_lotedetatrans,&
this.dw_envadetatrans,&
this.dw_granpesatrans,&
this.dw_granpesa,&
this.dw_envadeta,&
this.dw_lotedeta,&
this.dw_movtodeta,&
this.cb_1,&
this.dw_loteenca,&
this.dw_envaenca,&
this.dw_movtoencatrans,&
this.dw_movtoenca}
end on

on uo_traspaso_interplanta.destroy
destroy(this.dw_archivos)
destroy(this.dw_envaencatrans)
destroy(this.dw_loteencatrans)
destroy(this.dw_movtodetatrans)
destroy(this.dw_lotedetatrans)
destroy(this.dw_envadetatrans)
destroy(this.dw_granpesatrans)
destroy(this.dw_granpesa)
destroy(this.dw_envadeta)
destroy(this.dw_lotedeta)
destroy(this.dw_movtodeta)
destroy(this.cb_1)
destroy(this.dw_loteenca)
destroy(this.dw_envaenca)
destroy(this.dw_movtoencatrans)
destroy(this.dw_movtoenca)
end on

event destructor;//IF isvalid(SQLTRA) THEN 
//	Disconnect using SQLTRA;
//	DESTROY SQLTRA
//END IF
//
//DESTROY iuo_categoria;
//DESTROY iuo_frio;
//DESTROY iuo_condicion;
//DESTROY iuo_variedad;
//DESTROY iuo_envases;
//DESTROY iuo_camion;
//DESTROY iuo_especie;
//DESTROY iuo_planta;
//DESTROY iuo_productor;
//DESTROY iuo_cliente;
//DESTROY iuo_motivo;
//DESTROY iuo_transp;
//
//GarbageCollect()
end event

event constructor;//This.SetTransObject(SQLCA)

iuo_categoria		=	Create uo_categorias
iuo_frio				=	Create uo_periodofrio
iuo_condicion		=	Create uo_condicioncc	
iuo_variedad		=	Create uo_variedades
iuo_envases			=	Create uo_envases
iuo_camion			=	Create uo_camiones
iuo_especie			=	Create uo_especie
iuo_planta			=	Create uo_plantadesp
iuo_productor		=	Create uo_productores
iuo_cliente			=	Create uo_Clientesprod
iuo_motivo			=	Create uo_motivodespacho
iuo_transp			=	Create uo_transportista
iuo_bins				=	Create uo_bins
iuo_camarasbode	=	Create uo_camarasfrigo

dw_movtoencatrans.dataobject 	= "dw_mant_movtofrutagranenca_trans"
dw_movtodetatrans.dataobject 	= "dw_mues_movtofrutagraneldeta_trans"
dw_envaencatrans.dataobject 	= "dw_mant_movtoenvaenca_trans"
dw_envadetatrans.dataobject 	= "dw_mues_movtoenvadeta_trans"
dw_loteencatrans.dataobject 	= "dw_mues_spro_lotesfrutagranel_trans"
dw_lotedetatrans.dataobject 	= "dw_mues_spro_lotesfrutagradet_trans"
dw_granpesatrans.dataobject 	= "dw_pesaje_romana_trans"
//dw_movtobinstrans.dataobject 	= "dw_spro_movtobins_trans"

dw_archivos.dataobject 			= "dw_archivos"

dw_movtoenca.dataobject 		= "dw_mant_movtofrutagranenca_origen"
dw_movtodeta.dataobject 		= "dw_mues_movtofrutagraneldeta_origen"
dw_granpesa.dataobject 			= "dw_pesaje_romana_origen"
dw_envaenca.dataobject 			= "dw_mant_movtoenvaenca_origen"
dw_envadeta.dataobject 			= "dw_mues_movtoenvadeta_origen"
dw_loteenca.dataobject 			= "dw_mues_spro_lotesfrutagranel_origen"
dw_lotedeta.dataobject 			= "dw_mues_spro_lotesfrutagradet_origen"
//dw_movtobins.dataobject 		= "dw_spro_movtobins_origen"
end event

type dw_archivos from datawindow within uo_traspaso_interplanta
integer x = 1079
integer y = 1640
integer width = 1010
integer height = 252
integer taborder = 60
string title = "none"
string dataobject = "dw_archivos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_envaencatrans from datawindow within uo_traspaso_interplanta
integer x = 1079
integer y = 956
integer width = 1010
integer height = 216
integer taborder = 40
string title = "none"
string dataobject = "dw_mant_movtoenvaenca_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_loteencatrans from datawindow within uo_traspaso_interplanta
integer x = 1079
integer y = 492
integer width = 1010
integer height = 216
integer taborder = 20
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagranel_trans"
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_movtodetatrans from datawindow within uo_traspaso_interplanta
integer x = 1079
integer y = 260
integer width = 1010
integer height = 216
integer taborder = 30
string title = "none"
string dataobject = "dw_mues_movtofrutagraneldeta_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_lotedetatrans from datawindow within uo_traspaso_interplanta
integer x = 1079
integer y = 724
integer width = 1010
integer height = 216
integer taborder = 20
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagradet_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_envadetatrans from datawindow within uo_traspaso_interplanta
integer x = 1079
integer y = 1188
integer width = 1010
integer height = 216
integer taborder = 40
string title = "none"
string dataobject = "dw_mues_movtoenvadeta_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_granpesatrans from datawindow within uo_traspaso_interplanta
integer x = 1079
integer y = 1420
integer width = 1010
integer height = 216
integer taborder = 50
string title = "none"
string dataobject = "dw_pesaje_romana_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_granpesa from datawindow within uo_traspaso_interplanta
integer x = 37
integer y = 1420
integer width = 1010
integer height = 216
integer taborder = 50
string title = "none"
string dataobject = "dw_pesaje_romana_origen"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_envadeta from datawindow within uo_traspaso_interplanta
integer x = 37
integer y = 1188
integer width = 1010
integer height = 216
integer taborder = 40
string title = "none"
string dataobject = "dw_mues_movtoenvadeta_origen"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_lotedeta from datawindow within uo_traspaso_interplanta
integer x = 37
integer y = 724
integer width = 1010
integer height = 216
integer taborder = 20
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagradet_origen"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_movtodeta from datawindow within uo_traspaso_interplanta
integer x = 37
integer y = 260
integer width = 1010
integer height = 216
integer taborder = 30
string title = "none"
string dataobject = "dw_mues_movtofrutagraneldeta_origen"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type cb_1 from commandbutton within uo_traspaso_interplanta
integer x = 297
integer y = 1696
integer width = 485
integer height = 136
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Gatilla Traspaso"
end type

event clicked;Parent.TriggerEvent("TraspasaDatos")
end event

type dw_loteenca from datawindow within uo_traspaso_interplanta
integer x = 37
integer y = 492
integer width = 1010
integer height = 216
integer taborder = 10
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagranel_origen"
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_envaenca from datawindow within uo_traspaso_interplanta
integer x = 37
integer y = 956
integer width = 1010
integer height = 216
integer taborder = 30
string title = "none"
string dataobject = "dw_mant_movtoenvaenca_origen"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_movtoencatrans from datawindow within uo_traspaso_interplanta
integer x = 1079
integer y = 28
integer width = 1010
integer height = 216
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_movtofrutagranenca_trans"
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

type dw_movtoenca from datawindow within uo_traspaso_interplanta
integer x = 37
integer y = 28
integer width = 1010
integer height = 216
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_movtofrutagranenca_origen"
borderstyle borderstyle = stylelowered!
end type

event constructor;This.SetTransObject(SQLCA)
end event

