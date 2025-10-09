$PBExportHeader$w_maed_despafrigoen.srw
forward
global type w_maed_despafrigoen from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_despafrigoen
end type
type dw_4 from datawindow within w_maed_despafrigoen
end type
type dw_5 from datawindow within w_maed_despafrigoen
end type
type dw_7 from datawindow within w_maed_despafrigoen
end type
type dw_8 from datawindow within w_maed_despafrigoen
end type
type dw_9 from datawindow within w_maed_despafrigoen
end type
type dw_10 from datawindow within w_maed_despafrigoen
end type
type dw_11 from datawindow within w_maed_despafrigoen
end type
type dw_12 from datawindow within w_maed_despafrigoen
end type
type dw_13 from datawindow within w_maed_despafrigoen
end type
type dw_14 from datawindow within w_maed_despafrigoen
end type
type dw_15 from datawindow within w_maed_despafrigoen
end type
type dw_alpalletencab from datawindow within w_maed_despafrigoen
end type
type dw_alpalletfruta from datawindow within w_maed_despafrigoen
end type
type dw_palletencahisto from datawindow within w_maed_despafrigoen
end type
type dw_palletfrutahisto from datawindow within w_maed_despafrigoen
end type
type dw_archplano from datawindow within w_maed_despafrigoen
end type
type dw_16 from datawindow within w_maed_despafrigoen
end type
type dw_6 from datawindow within w_maed_despafrigoen
end type
type dw_inspeccion_enc from datawindow within w_maed_despafrigoen
end type
type dw_inspeccion_deta from datawindow within w_maed_despafrigoen
end type
type cb_limpia from commandbutton within w_maed_despafrigoen
end type
end forward

global type w_maed_despafrigoen from w_mant_encab_deta_csd
integer width = 3607
integer height = 2276
string title = "Depachos"
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = child!
event ue_imprimir ( )
event ue_despuesguardar ( )
event ue_despuesgrabar ( )
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_7 dw_7
dw_8 dw_8
dw_9 dw_9
dw_10 dw_10
dw_11 dw_11
dw_12 dw_12
dw_13 dw_13
dw_14 dw_14
dw_15 dw_15
dw_alpalletencab dw_alpalletencab
dw_alpalletfruta dw_alpalletfruta
dw_palletencahisto dw_palletencahisto
dw_palletfrutahisto dw_palletfrutahisto
dw_archplano dw_archplano
dw_16 dw_16
dw_6 dw_6
dw_inspeccion_enc dw_inspeccion_enc
dw_inspeccion_deta dw_inspeccion_deta
cb_limpia cb_limpia
end type
global w_maed_despafrigoen w_maed_despafrigoen

type variables
w_mant_deta_despafrigode iw_mantencion

DataWindowChild	dw_planta, dw_plades, dw_puerto,dw_etiquetas,dw_plantas,dw_clientes,&
						dw_ptodes,dw_sitios,idwc_patente,idwc_multipuerto,idwc_especie,idwc_cargotecnico

Transaction	sqlconec
Transaction	sqlconec2

Boolean		ib_conectado, ib_existe_folioD, ib_conectado2
Integer     	ii_tipoin, ii_secuen, ii_controlaaceso, ii_existe, ii_blockcont, il_destino, il_desreferencia,&
				il_existepal, il_control, il_cierra, il_existetrans, il_palexistencia, il_exisrecepcion, il_despadestino,&
				il_exisdettrans, il_frena
Long      		il_numins, il_Folio
String			is_rutclie, is_rutchofer

DataStore	ids_CorrelMovim, ids_palletfruta_fecha

uo_patente				iuo_patente
uo_especie				iuo_especie
uo_clienprove			iuo_clprv
uo_calibre				iuo_calibre
uo_transportista		iuo_Transportista
uo_transportista		iuo_TransportistaServ
uo_ClientesProd		iuo_Cliente
uo_guiadespacho		iuo_Guia
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string columna)
public subroutine buscaembarque ()
public function boolean conexionbase ()
public function long buscanuevofolio (integer planta)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean wf_actualiza_carga (boolean borrando)
public function boolean busca_numeroinspeccion (integer cliente, integer planta, long pallet)
public subroutine existe_cargaregistro ()
public subroutine generaarchivoplano ()
public function long busnuevofoliodespa (integer ai_cliente, integer ai_planta)
public function boolean existefolio (string as_columna, string as_valor)
public function boolean buscaregistros ()
public function boolean coneccionbase ()
public subroutine enviamail ()
public function string rescatacorreo (integer ai_planta)
public function boolean buscasellos (long sello)
protected function boolean noexisteembarque (string as_embarque)
public function boolean buscadestino (string as_embarque, integer ai_tratamiento)
public subroutine buscacliente ()
public function boolean existe_plasag (integer ai_planta, long ai_plasag)
public function boolean existe_guianula (integer ai_cliente, long al_guia, long al_despacho)
public subroutine cambiaestadopallet (integer ai_cliente, integer ai_planta, long al_pallet, integer ai_cajas)
public function boolean pallet_existencia (integer ai_cliente, integer ai_planta, long al_pallet)
public function boolean existe_pallet (integer ai_cliente, integer ai_planta, long al_pallet, long al_guia)
public function boolean planillaanulada (integer ai_planta, long ai_plasag, string ai_tipoplani)
public subroutine carga_inspeccion (long pallet, long fila, integer ai_destino)
public subroutine genera_archico_inspeccion ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "DESPACHO DE FRUTA PROCESADA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_despafrigoen"

vinf.dw_1.GetChild("plde_codigo", dw_plantas)
vinf.dw_1.GetChild("etiq_codigo", dw_etiquetas)
vinf.dw_1.GetChild("clie_codigo", dw_clientes)

dw_plantas.SetTransObject(sqlca)
dw_etiquetas.SetTransObject(sqlca)
dw_clientes.SetTransObject(sqlca)

dw_plantas.Retrieve(1)
dw_etiquetas.Retrieve()
dw_clientes.Retrieve()

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Zoom = 90')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_despuesguardar();//
Long ll_fila, ll_Pfruta, ll_numins, ll_fila2, ll_filanew, ll_destino, ll_pallet,ll_cont, ll_cajas, ll_cuenta
Integer	li_control, li_resultado, li_cajas, li_plandest, li_cliente, li_planta, li_ctlventana
String	ls_tpem_codigo, ls_embalaje

il_control = 0

SetPointer(HourGlass!)

Open(w_mensaje)
w_mensaje.st_1.text = " Transmisión Automática en Curso"
	
dw_3.SetTransObject(sqlconec)
dw_4.SetTransObject(sqlconec)
dw_7.SetTransObject(sqlconec)
dw_8.SetTransObject(sqlconec)
dw_11.SetTransObject(sqlconec)
dw_12.SetTransObject(sqlconec)
dw_15.SetTransObject(sqlconec)
dw_16.SetTransObject(sqlconec)

dw_3.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlconec)
dw_planta.Retrieve(1)

dw_3.GetChild("defe_plades", dw_plades)
dw_plades.SetTransObject(sqlconec)
dw_plades.Retrieve(1)

dw_3.GetChild("puer_codigo", dw_puerto)
dw_puerto.SetTransObject(sqlconec)
dw_puerto.Retrieve(1)

dw_5.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlca)
dw_planta.Retrieve(1)

IF dw_1.Rowcount() > 0 THEN
	FOR ll_Fila = 1 TO dw_1.Rowcount()
		IF existe_pallet(dw_1.Object.clie_codigo[1],dw_2.Object.defe_plades[1],dw_1.Object.paen_numero[ll_Fila],dw_2.Object.defe_guides[1]) THEN
			Close(w_mensaje)	
			Message.DoubleParm = -1
			il_frena = 1
			Return 
		ELSE
			IF il_palexistencia = 1  THEN
				MessageBox("Atención","Pallet "+String(dw_1.Object.paen_numero[ll_Fila])+" en existencia en planta destino. No es posible traspasarlo.",Exclamation!, Ok!, 2)
				il_frena = 1
			ELSE	
				IF il_despadestino = 1 THEN
					MessageBox("Atención","Pallet "+String(dw_1.Object.paen_numero[ll_Fila])+"  se encuentra despachado a puerto en planta destino.", Exclamation!, Ok!)
					il_frena = 1
				ELSE	
					IF il_exisrecepcion = 0 THEN
						IF dw_3.RowCount() = 0 THEN
							dw_3.Insertrow(0)
							dw_3.Object.plde_codigo[1]  = dw_2.Object.defe_plades[1]
							dw_3.Object.rfpe_ptaori[1]  = dw_2.Object.plde_codigo[1]
							dw_3.Object.rfpe_numero[1]  = dw_2.Object.defe_guides[1]   
							dw_3.Object.clie_codigo[1]  = dw_2.Object.clie_codigo[1]
							dw_3.Object.rfpe_fecrec[1]  = Today()
							dw_3.Object.rfpe_tipoen[1]  = 2
							dw_3.Object.rfpe_tarjas[1]  = dw_2.Object.defe_cantar[1]
							dw_3.Object.rfpe_guides[1]  = dw_2.Object.defe_guides[1]
							dw_3.Object.rfpe_nrores[1]  = dw_2.Object.defe_guides[1]
							dw_3.Object.tran_codigo[1]  = dw_2.Object.tran_codigo[1]
							dw_3.Object.tica_codigo[1]  = dw_2.Object.tica_codigo[1]
							dw_3.Object.rfpe_patent[1]  = dw_2.Object.defe_patent[1]
							dw_3.Object.tica_codigo[1]  = dw_2.Object.tica_codigo[1]
							dw_3.Object.rfpe_chofer[1]  = dw_2.Object.defe_chofer[1]
							dw_3.Object.rfpe_fecact[1]  = dw_2.Object.defe_fecact[1]
							dw_3.Object.rfpe_horact[1]  = dw_2.Object.defe_horact[1]
							dw_3.Object.rfpe_fecing[1]	 = dw_2.Object.defe_fecing[1]
						END IF	
					
						ll_filanew = dw_4.Insertrow(0)
						dw_4.Object.plde_codigo[ll_filanew] = dw_2.Object.defe_plades[1]
						dw_4.Object.rfpe_numero[ll_filanew]  = dw_2.Object.defe_guides[1]   
						dw_4.Object.clie_codigo[ll_filanew] = dw_2.Object.clie_codigo[1]
						dw_4.Object.paen_numero[ll_filanew] = dw_1.Object.paen_numero[ll_fila]
						
					ELSE
						IF il_exisdettrans = 0 THEN
							ll_filanew = dw_4.Insertrow(0)
							dw_4.Object.plde_codigo[ll_filanew] = dw_2.Object.defe_plades[1]
							dw_4.Object.rfpe_numero[ll_filanew]  = dw_2.Object.defe_guides[1]   
							dw_4.Object.clie_codigo[ll_filanew] = dw_2.Object.clie_codigo[1]
							dw_4.Object.paen_numero[ll_filanew] = dw_1.Object.paen_numero[ll_fila]
						END IF	
					END IF
										
					dw_5.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.Object.plde_codigo[1])
					IF dw_5.RowCount() > 0 THEN
						
						IF il_existetrans = 0 THEN
							li_cliente	= dw_2.Object.clie_codigo[1]
							li_planta	= dw_2.Object.plde_codigo[1]
							ll_pallet	= dw_1.Object.paen_numero[ll_fila]
							
							SELECT count(*)
							INTO :li_ctlventana
							FROM dbo.spro_controlventana 
							WHERE paen_numero = :ll_pallet
							AND clie_codigo = :li_cliente
							AND plde_codigo = :li_planta;
														
							dw_7.Object.paen_estado[ll_fila] = 1
							dw_7.Object.plde_codigo[ll_fila] = dw_2.Object.defe_plades[1]
							dw_7.Object.clie_codigo[ll_fila]	= dw_5.Object.clie_codigo[1]
							dw_7.Object.paen_numero[ll_fila] = dw_5.Object.paen_numero[1]   
							dw_7.Object.paen_tipopa[ll_fila] = dw_5.Object.paen_tipopa[1]   
							dw_7.Object.espe_codigo[ll_fila] = dw_5.Object.espe_codigo[1]   
							dw_7.Object.vari_codigo[ll_fila] = dw_5.Object.vari_codigo[1]   
							dw_7.Object.tiem_codigo[ll_fila] = dw_5.Object.tiem_codigo[1]   
							dw_7.Object.emba_codigo[ll_fila] = dw_5.Object.emba_codigo[1] 
							dw_7.Object.cate_codigo[ll_fila] = dw_5.Object.cate_codigo[1]   
							dw_7.Object.etiq_codigo[ll_fila] = dw_5.Object.etiq_codigo[1]   
							dw_7.Object.stat_codigo[ll_fila] = dw_5.Object.stat_codigo[1]   
							dw_7.Object.trat_codigo[ll_fila] = dw_5.Object.trat_codigo[1]   
							dw_7.Object.frio_codigo[ll_fila] = dw_5.Object.frio_codigo[1]   
							dw_7.Object.cond_codigo[ll_fila] = dw_5.Object.cond_codigo[1]   
							dw_7.Object.paen_cosecha[ll_fila]= dw_5.Object.paen_cosecha[1]    
							dw_7.Object.paen_altura[ll_fila] = dw_5.Object.paen_altura[1]   
							dw_7.Object.tmvp_codigo[ll_fila] = dw_5.Object.tmvp_codigo[1]   
							dw_7.Object.paen_fecini[ll_fila] = dw_5.Object.paen_fecini[1]   
							dw_7.Object.paen_horain[ll_fila] = dw_5.Object.paen_horain[1]   
							dw_7.Object.cama_codigo[ll_fila] = dw_5.Object.cama_codigo[1]   
							dw_7.Object.paen_calle[ll_fila]  = dw_5.Object.paen_calle[1]   
							dw_7.Object.paen_base[ll_fila]   = dw_5.Object.paen_base[1] 
							dw_7.Object.paen_posici[ll_fila] = dw_5.Object.paen_posici[1]   
							dw_7.Object.paen_estado[ll_fila] = 1   
							dw_7.Object.paen_fecemb[ll_fila] = dw_5.Object.paen_fecemb[1]   
							dw_7.Object.tpem_codigo[ll_fila] = dw_5.Object.tpem_codigo[1]   
							dw_7.Object.paen_ccajas[ll_fila] = dw_5.Object.paen_ccajas[1]   
							dw_7.Object.paen_concal[ll_fila] = dw_5.Object.paen_concal[1]   
							dw_7.Object.paen_inspec[ll_fila] = dw_5.Object.paen_inspec[1]   
							dw_7.Object.dest_codigo[ll_fila] = dw_5.Object.dest_codigo[1]   
							dw_7.Object.paen_pexpor[ll_fila] = dw_5.Object.paen_pexpor[1]   
							dw_7.Object.paen_pmixto[ll_fila] = dw_5.Object.paen_pmixto[1]   
							dw_7.Object.paen_varrot[ll_fila] = dw_5.Object.paen_varrot[1]   
							dw_7.Object.paen_nrasda[ll_fila] = dw_5.Object.paen_nrasda[1]   
							dw_7.Object.copa_codigo[ll_fila] = dw_5.Object.copa_codigo[1]    
							dw_7.Object.inpe_numero[ll_fila]	= dw_5.Object.inpe_numero[1]
							dw_7.Object.inpe_tipoin[ll_fila]	= dw_5.Object.inpe_tipoin[1]
							dw_7.Object.inpe_fechai[ll_fila]	= dw_5.Object.inpe_fechai[1]
							dw_7.Object.paen_pcopda[ll_fila]	= dw_5.Object.paen_pcopda[1]
							IF li_ctlventana > 0 THEN
								dw_7.Object.paen_venimp[ll_fila] = 1
							ELSE	
								dw_7.Object.paen_venimp[ll_fila] = 0
							END IF	
							
							ls_embalaje	= dw_5.Object.emba_codigo[1]
							ll_cajas		= dw_5.Object.paen_ccajas[1] 	
													  	
						  	SELECT count(*)
							  INTO :ll_cuenta
							  FROM dbo.tipopallemba
							  WHERE clie_codigo = :li_cliente
								  AND emba_codigo = :ls_embalaje
								  AND tpem_codigo = :ll_cajas
								  Using (sqlconec);
								  
							IF ll_cont = 0 THEN	  
								INSERT INTO dbo.tipopallemba(clie_codigo,emba_codigo,tpem_codigo,tpem_cancaj,tpem_altura)
								VALUES (:li_cliente,:ls_embalaje,:ll_cajas,:ll_cajas,213)
								Using (sqlconec);
							END IF		  
								  
							li_cliente	= dw_2.Object.clie_codigo[1]
							li_planta	= dw_2.Object.plde_codigo[1]
							ll_pallet	= dw_1.Object.paen_numero[ll_fila]
												
						ELSE
							ll_cont = dw_16.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.Object.defe_plades[1])
							
							IF ll_cont > 0 THEN
								//dw_16.Object.paen_ccajas[1] = dw_5.Object.paen_ccajas[1]
								IF dw_5.Object.paen_tipopa[1] = 1 THEN
									ls_tpem_codigo = dw_5.Object.tpem_codigo[1]
									ll_cajas 	=  dw_5.Object.paen_ccajas[1]
									li_cliente	= dw_2.Object.clie_codigo[1]
									li_planta	= dw_2.Object.defe_plades[1]
									ll_pallet	= dw_1.Object.paen_numero[ll_fila]
									ls_embalaje	= dw_5.Object.emba_codigo[1]
																		
									UPDATE dbo.palletencab_trans SET
									paen_ccajas = :ll_cajas,
									tpem_codigo = :ls_tpem_codigo
									WHERE clie_Codigo = :li_cliente
									AND plde_codigo = :li_planta
									AND paen_numero = :ll_pallet
									Using (sqlconec);
									
									IF sqlconec.SQLCode = -1 THEN
										F_errorbasedatos(sqlconec,"Lectura tabla palletencab_trans")
										RETURN 
									END IF	
																		
									SELECT count(*)
									  INTO :ll_cuenta
									  FROM dbo.tipopallemba
									  WHERE clie_codigo = :li_cliente
										  AND emba_codigo = :ls_embalaje
										  AND tpem_codigo = :ll_cajas
										  Using (sqlconec);
										  
									IF ll_cont = 0 THEN	  
										INSERT INTO dbo.tipopallemba(clie_codigo,emba_codigo,tpem_codigo,tpem_cancaj,tpem_altura)
										VALUES (:li_cliente,:ls_embalaje,:ll_cajas,:ll_cajas,213)
										Using (sqlconec);
									END IF		
																
								ELSE
									li_cliente	= dw_2.Object.clie_codigo[1]
									li_planta	= dw_2.Object.defe_plades[1]
									ll_pallet	= dw_1.Object.paen_numero[ll_fila]
																	
									UPDATE dbo.palletencab_trans SET
									paen_ccajas = :ll_cajas
									WHERE clie_Codigo = :li_cliente
									AND plde_codigo = :li_planta
									AND paen_numero = :ll_pallet
									Using (sqlconec);
									IF sqlconec.SQLCode = -1 THEN
										F_errorbasedatos(sqlconec,"Lectura tabla palletencab_trans")
										RETURN
									END IF	
									//	dw_16.Object.tpem_codigo[ll_fila] = dw_5.Object.tpem_codigo[1]
								END IF	
//								IF dw_16.Update(True, False)	=	1	THEN
//									Commit;
//										
//									IF sqlconec.SQLCode <> 0 THEN
//										F_ErrorBaseDatos(sqlconec, This.Title)
//										RollBack;
//									ELSE
//										dw_16.ResetUpdate()										
//									END IF
//								ELSE
//									F_ErrorBaseDatos(sqlconec, This.Title)
//									RollBack;
//								END IF
							END IF	
						END IF			
					END IF
					
					dw_16.Reset()
					ll_destino = dw_2.Object.defe_plades[1]
					  
					SELECT isnull(plde_cajbul,0) 
					INTO :li_control  
					FROM dbo.plantadesp
					WHERE plde_codigo = :ll_destino;
					
					dw_6.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.Object.plde_codigo[1],li_control)
				
					IF dw_6.RowCount() > 0 THEN
						FOR ll_Pfruta = 1 TO dw_6.RowCount()
							ll_fila2	=	dw_8.InsertRow(0)
							dw_8.Object.plde_codigo[ll_fila2] = dw_2.Object.defe_plades[1]
							dw_8.Object.clie_codigo[ll_fila2] = dw_6.Object.clie_codigo[ll_Pfruta]
							dw_8.Object.paen_numero[ll_fila2] = dw_6.Object.paen_numero[ll_Pfruta]
							dw_8.Object.espe_codigo[ll_fila2] = dw_6.Object.espe_codigo[ll_Pfruta]
							dw_8.Object.vari_codigo[ll_fila2] = dw_6.Object.vari_codigo[ll_Pfruta]
							dw_8.Object.pafr_varrot[ll_fila2] = dw_6.Object.pafr_varrot[ll_Pfruta]
							dw_8.Object.emba_codigo[ll_fila2] = dw_6.Object.emba_codigo[ll_Pfruta]
							dw_8.Object.prod_codigo[ll_fila2] = dw_6.Object.prod_codigo[ll_Pfruta]
							dw_8.Object.pafr_prdrot[ll_fila2] = dw_6.Object.pafr_prdrot[ll_Pfruta]
							dw_8.Object.cond_codigo[ll_fila2] = dw_6.Object.cond_codigo[ll_Pfruta]
							dw_8.Object.etiq_codigo[ll_fila2] = dw_6.Object.etiq_codigo[ll_Pfruta]
							dw_8.Object.pafr_calibr[ll_fila2] = dw_6.Object.pafr_calibr[ll_Pfruta]
							dw_8.Object.pafr_calrot[ll_fila2] = dw_6.Object.pafr_calrot[ll_Pfruta]
							dw_8.Object.pafr_secuen[ll_fila2] = dw_6.Object.pafr_secuen[ll_Pfruta]
							dw_8.Object.pafr_ccajas[ll_fila2] = dw_6.Object.pafr_ccajas[ll_Pfruta]
							dw_8.Object.pafr_nrlote[ll_fila2] = dw_6.Object.pafr_nrlote[ll_Pfruta]
							dw_8.Object.pafr_copack[ll_fila2] = dw_6.Object.pafr_copack[ll_Pfruta] 
							dw_8.Object.pafr_rotpak[ll_fila2] = dw_6.Object.pafr_rotpak[ll_Pfruta] 
							dw_8.Object.pafr_huert1[ll_fila2] = dw_6.Object.pafr_huert1[ll_Pfruta]
							dw_8.Object.pafr_cuart1[ll_fila2] = dw_6.Object.pafr_cuart1[ll_Pfruta]
							dw_8.Object.pafr_huert2[ll_fila2] = dw_6.Object.pafr_huert2[ll_Pfruta]
							dw_8.Object.pafr_cuart2[ll_fila2] = dw_6.Object.pafr_cuart2[ll_Pfruta]
							dw_8.Object.pafr_huert4[ll_fila2] = dw_6.Object.pafr_huert4[ll_Pfruta]
							dw_8.Object.pafr_cuart4[ll_fila2] = dw_6.Object.pafr_cuart4[ll_Pfruta]
							dw_8.Object.pafr_fecemb[ll_fila2] = dw_6.Object.pafr_fecemb[ll_Pfruta]
							dw_8.Object.pafr_fecing[ll_fila2] = dw_6.Object.pafr_fecing[ll_Pfruta]
							//dw_8.Object.pafr_docrel[ll_fila2] = dw_6.Object.pafr_docrel[ll_Pfruta]
							dw_8.Object.PAFR_HUERT3[ll_fila2] = dw_6.Object.PAFR_HUERT3[ll_Pfruta]
							dw_8.Object.PAFR_CUART3[ll_fila2] = dw_6.Object.PAFR_CUART3[ll_Pfruta]
							dw_8.Object.PAFR_CUART4[ll_fila2] = dw_6.Object.PAFR_CUART4[ll_Pfruta]
							dw_8.Object.PAFR_BARRA1[ll_fila2] = dw_6.Object.PAFR_BARRA1[ll_Pfruta]
							dw_8.Object.PAFR_BARRA2[ll_fila2]  = dw_6.Object.PAFR_BARRA2[ll_Pfruta]
							dw_8.Object.PAFR_BARRA3[ll_fila2] = dw_6.Object.PAFR_BARRA3[ll_Pfruta]
							dw_8.Object.PAFR_BARRA4[ll_fila2] = dw_6.Object.PAFR_BARRA4[ll_Pfruta]
							dw_8.Object.pafr_cjssal[ll_fila2] = dw_6.Object.pafr_cjssal[ll_Pfruta]
							dw_8.Object.pafr_docrel[ll_fila2] = dw_6.Object.pafr_docrel[ll_Pfruta]
							dw_8.Object.pafr_embrea[ll_fila2] = dw_6.Object.pafr_embrea[ll_Pfruta]
							dw_8.Object.pafr_fecrot[ll_fila2] = dw_6.Object.pafr_fecrot[ll_Pfruta]
							dw_8.Object.cama_nombre[ll_fila2] = dw_6.Object.cama_nombre[ll_Pfruta]
							dw_8.Object.cate_codigo[ll_fila2] = dw_6.Object.cate_codigo[ll_Pfruta]
							dw_8.Object.pafr_catrot[ll_fila2] = dw_6.Object.pafr_catrot[ll_Pfruta]
					
						NEXT
						
						li_plandest = dw_2.Object.defe_plades[1]
						li_cliente  = dw_2.Object.clie_codigo[1]
						ll_pallet   = dw_1.Object.paen_numero[ll_fila]
						
						DELETE dbo.palletfruta_trans
						WHERE clie_codigo = :li_cliente
						AND plde_codigo = :li_plandest
						AND paen_numero = :ll_pallet
						Using (sqlconec);
						
					END IF
					dw_14.Retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[ll_fila])
					IF dw_14.RowCount() > 0 THEN
						FOR ll_Pfruta = 1 TO dw_14.RowCount()
							ll_Fila2	=	dw_15.InsertRow(0)
							dw_15.Object.clie_codigo[ll_Fila2]	=	dw_14.Object.clie_codigo[ll_Pfruta]					
							dw_15.Object.plde_codigo[ll_Fila2]	=	dw_14.Object.capr_cespak[ll_Pfruta]	//dw_2.Object.plde_codigo[1]//dw_2.Object.defe_plades[1]
							dw_15.Object.capr_numero[ll_Fila2]	=	dw_14.Object.capr_numero[ll_Pfruta]	
							dw_15.Object.espe_codigo[ll_Fila2]	=	dw_14.Object.espe_codigo[ll_Pfruta]	
							dw_15.Object.vari_codigo[ll_Fila2]	=	dw_14.Object.vari_codigo[ll_Pfruta]						
							dw_15.Object.prod_codigo[ll_Fila2]	=	dw_14.Object.prod_codigo[ll_Pfruta]						
							dw_15.Object.prod_predio[ll_Fila2]	=	dw_14.Object.prod_predio[ll_Pfruta]	
							dw_15.Object.prod_huerto[ll_Fila2]	=	dw_14.Object.prod_huerto[ll_Pfruta]						
							dw_15.Object.prod_cuarte[ll_Fila2]	=	dw_14.Object.prod_cuarte[ll_Pfruta]	
							dw_15.Object.emba_codigo[ll_Fila2]	=	dw_14.Object.emba_codigo[ll_Pfruta]						
							dw_15.Object.etiq_codigo[ll_Fila2]	=	dw_14.Object.etiq_codigo[ll_Pfruta]						
							dw_15.Object.capr_fecemb[ll_Fila2]	=	dw_14.Object.capr_fecemb[ll_Pfruta]	
							dw_15.Object.capr_calibr[ll_Fila2]	=	dw_14.Object.capr_calibr[ll_Pfruta]	
							dw_15.Object.capr_embala[ll_Fila2]	=	dw_14.Object.capr_embala[ll_Pfruta]						
							dw_15.Object.capr_selecc[ll_Fila2]	=	dw_14.Object.capr_selecc[ll_Pfruta]	
							dw_15.Object.capr_pesado[ll_Fila2]	=	dw_14.Object.capr_pesado[ll_Pfruta]
							dw_15.Object.capr_cean14[ll_Fila2]	=	dw_14.Object.capr_cean14[ll_Pfruta]	
							dw_15.Object.capr_numpal[ll_Fila2]	=	dw_1.Object.paen_numero[ll_Fila]   //dw_14.Object.capr_numpal[ll_Pfruta]	
							dw_15.Object.capr_regcap[ll_Fila2]	=	dw_14.Object.capr_regcap[ll_Pfruta]
							dw_15.Object.capr_estado[ll_Fila2]	=	dw_14.Object.capr_estado[ll_Pfruta]	
							dw_15.Object.capr_varrot[ll_Fila2]	=	dw_14.Object.capr_varrot[ll_Pfruta]	
							dw_15.Object.capr_numgia[ll_Fila2]	=	dw_14.Object.capr_numgia[ll_Pfruta]
							dw_15.Object.cate_codigo[ll_Fila2]	=	dw_14.Object.cate_codigo[ll_Pfruta]	
							dw_15.Object.capr_cespak[ll_Fila2]	=	dw_14.Object.capr_cespak[ll_Pfruta]	
						//	dw_15.Object.capr_docrel[ll_Fila2]	=	dw_14.Object.capr_docrel[ll_Pfruta]	
							dw_15.Object.capr_hordig[ll_Fila2]	=	dw_14.Object.capr_hordig[ll_Pfruta]	
							dw_15.Object.capr_fecdig[ll_Fila2]	=	dw_14.Object.capr_fecdig[ll_Pfruta]	
							dw_15.Object.capr_nrlote[ll_Fila2]	=	dw_14.Object.capr_nrlote[ll_Pfruta]
							dw_15.Object.capr_catrot[ll_Fila2]	=	dw_14.Object.capr_catrot[ll_Pfruta]
							
						NEXT
							
						dw_6.Reset()
						//dw_5.Reset()
						dw_14.Reset()		
						dw_16.Reset()		 
					END IF	
					IF dw_1.Object.paen_inspec[ll_fila] = 1 THEN
						IF Busca_Numeroinspeccion(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[ll_fila]) THEN
							carga_inspeccion(dw_1.Object.paen_numero[ll_fila],ll_fila,dw_5.Object.dest_codigo[1])
							dw_5.Reset()
						END IF
					END IF
					dw_5.Reset()
				END IF	
			END IF
		END IF	
	NEXT		
END IF

Close(w_mensaje)	

end event

event ue_despuesgrabar();Long ll_fila2, ll_nueva2, ll_numero, ll_nueva, ll_fila1, ll_pallet,&
		ll_nueva1, ll_nueva3, ll_fila3, ll_despacho, ll_destino
Integer	li_cliente, li_planta, li_control
Boolean	lb_AutoCommit, lb_Retorno

SetPointer(HourGlass!)

li_cliente 	= dw_2.Object.clie_codigo[1]
li_planta 	= dw_2.Object.plde_codigo[1]
ll_despacho = dw_2.Object.defe_numero[1]

Select max(altu_numero) + 1
into :ll_numero
from dbo.alpalletencab
where clie_codigo = :li_cliente
and	plde_codigo = :li_planta
and   altu_numero < 99999999;

IF isnull(ll_numero) OR ll_numero = 0 THEN
	ll_numero = 1
END IF	

ll_nueva = dw_alpalletencab.InsertRow(0)

dw_alpalletencab.Object.clie_codigo[ll_nueva] = li_cliente  
dw_alpalletencab.Object.plde_codigo[ll_nueva] = li_planta
dw_alpalletencab.Object.altu_numero[ll_nueva] = ll_numero
dw_alpalletencab.Object.altu_fecmov[ll_nueva] = dw_2.Object.defe_fecdes[1]
dw_alpalletencab.Object.altu_observ[ll_nueva] = 'Despacho'

ll_destino = dw_2.Object.defe_plades[1]
			  
SELECT isnull(plde_cajbul,0) 
INTO :li_control  
FROM dbo.plantadesp
WHERE plde_codigo = :ll_destino;

FOR ll_fila1 = 1 TO dw_1.RowCount() 
	ll_pallet = dw_1.Object.paen_numero[ll_fila1]

	ll_nueva1 = dw_alpalletfruta.InsertRow(0)
	
	dw_alpalletfruta.Object.clie_codigo[ll_nueva1] = li_cliente  
	dw_alpalletfruta.Object.plde_codigo[ll_nueva1] = li_planta
	dw_alpalletfruta.Object.altu_numero[ll_nueva1] = ll_numero
	dw_alpalletfruta.Object.alpf_fecmov[ll_nueva1] = dw_2.Object.defe_fecdes[1]
	dw_alpalletfruta.Object.paen_numero[ll_nueva1] = ll_pallet
		
	dw_5.Retrieve(li_cliente,ll_pallet,li_planta)
	dw_6.Retrieve(li_cliente,ll_pallet,li_planta,li_control)
	
	FOR ll_fila2 = 1 TO dw_5.RowCount()
		ll_nueva2 = dw_palletencahisto.InsertRow(0)
	
		dw_palletencahisto.Object.clie_codigo[ll_nueva2] = li_cliente   
		dw_palletencahisto.Object.paen_numero[ll_nueva2] = dw_5.Object.paen_numero[ll_fila2]  
		dw_palletencahisto.Object.plde_codigo[ll_nueva2] = li_planta  
		dw_palletencahisto.Object.pahi_proces[ll_nueva2] = ll_numero  
		dw_palletencahisto.Object.pahi_tipopa[ll_nueva2] = dw_5.Object.paen_tipopa[ll_fila2]  
		dw_palletencahisto.Object.tpem_codigo[ll_nueva2] = dw_5.Object.tpem_codigo[ll_fila2]     
		dw_palletencahisto.Object.espe_codigo[ll_nueva2] = dw_5.Object.espe_codigo[ll_fila2]    
		dw_palletencahisto.Object.vari_codigo[ll_nueva2] = dw_5.Object.vari_codigo[ll_fila2]    
		dw_palletencahisto.Object.tiem_codigo[ll_nueva2] = dw_5.Object.tiem_codigo[ll_fila2]    
		dw_palletencahisto.Object.emba_codigo[ll_nueva2] = dw_5.Object.emba_codigo[ll_fila2]    
		dw_palletencahisto.Object.cate_codigo[ll_nueva2] = dw_5.Object.cate_codigo[ll_fila2]    
		dw_palletencahisto.Object.etiq_codigo[ll_nueva2] = dw_5.Object.etiq_codigo[ll_fila2]    
		dw_palletencahisto.Object.stat_codigo[ll_nueva2] = dw_5.Object.stat_codigo[ll_fila2]    
		dw_palletencahisto.Object.trat_codigo[ll_nueva2] = dw_5.Object.trat_codigo[ll_fila2]    
		dw_palletencahisto.Object.frio_codigo[ll_nueva2] = dw_5.Object.frio_codigo[ll_fila2]    
		dw_palletencahisto.Object.cond_codigo[ll_nueva2] = dw_5.Object.cond_codigo[ll_fila2]    
		dw_palletencahisto.Object.dest_codigo[ll_nueva2] = dw_5.Object.dest_codigo[ll_fila2]    
		dw_palletencahisto.Object.pahi_fecemb[ll_nueva2] = dw_5.Object.paen_fecemb[ll_fila2]    
		dw_palletencahisto.Object.pahi_cosecha[ll_nueva2] = dw_5.Object.paen_cosecha[ll_fila2]     
		dw_palletencahisto.Object.paen_altura[ll_nueva2] = dw_5.Object.paen_altura[ll_fila2]    
		dw_palletencahisto.Object.paen_ccajas[ll_nueva2] = dw_5.Object.paen_ccajas[ll_fila2]    
		dw_palletencahisto.Object.tmvp_codigo[ll_nueva2] = dw_5.Object.tmvp_codigo[ll_fila2]    
		dw_palletencahisto.Object.paen_fecini[ll_nueva2] = dw_5.Object.paen_fecini[ll_fila2]    
		dw_palletencahisto.Object.paen_horain[ll_nueva2] = dw_5.Object.paen_horain[ll_fila2]    
		dw_palletencahisto.Object.cama_codigo[ll_nueva2] = dw_5.Object.cama_codigo[ll_fila2]    
		dw_palletencahisto.Object.pahi_calle[ll_nueva2]  = dw_5.Object.paen_calle[ll_fila2]    
		dw_palletencahisto.Object.pahi_base[ll_nueva2] 	 = dw_5.Object.paen_base[ll_fila2]   
		dw_palletencahisto.Object.pahi_posici[ll_nueva2] = dw_5.Object.paen_posici[ll_fila2]    
		dw_palletencahisto.Object.pahi_estado[ll_nueva2] = dw_5.Object.paen_estado[ll_fila2]    
		dw_palletencahisto.Object.pahi_inspec[ll_nueva2] = dw_5.Object.paen_inspec[ll_fila2]    
		dw_palletencahisto.Object.pahi_concal[ll_nueva2] = dw_5.Object.paen_concal[ll_fila2]    
		dw_palletencahisto.Object.pahi_pexpor[ll_nueva2] = dw_5.Object.paen_pexpor[ll_fila2]
		dw_palletencahisto.Object.pahi_pmixto[ll_nueva2] = dw_5.Object.paen_pmixto[ll_fila2]
	NEXT
	
	FOR ll_fila3 = 1 TO dw_6.RowCount() 
	
		ll_nueva3 = dw_palletfrutahisto.InsertRow(0)

		dw_palletfrutahisto.Object.clie_codigo[ll_nueva3] = li_cliente 
		dw_palletfrutahisto.Object.paen_numero[ll_nueva3] = dw_6.Object.paen_numero[ll_fila3]    
		dw_palletfrutahisto.Object.espe_codigo[ll_nueva3] = dw_6.Object.espe_codigo[ll_fila3]  
		dw_palletfrutahisto.Object.vari_codigo[ll_nueva3] = dw_6.Object.vari_codigo[ll_fila3]    
		dw_palletfrutahisto.Object.emba_codigo[ll_nueva3] = dw_6.Object.emba_codigo[ll_fila3]    
		dw_palletfrutahisto.Object.prod_codigo[ll_nueva3] = dw_6.Object.prod_codigo[ll_fila3]    
		dw_palletfrutahisto.Object.cond_codigo[ll_nueva3] = dw_6.Object.cond_codigo[ll_fila3]    
		dw_palletfrutahisto.Object.etiq_codigo[ll_nueva3] = dw_6.Object.etiq_codigo[ll_fila3]     
		dw_palletfrutahisto.Object.plde_codigo[ll_nueva3] = li_planta  
		dw_palletfrutahisto.Object.pafh_calibr[ll_nueva3] = dw_6.Object.pafr_calibr[ll_fila3]    
		dw_palletfrutahisto.Object.pafh_proces[ll_nueva3] = ll_numero 
		IF li_control = 0 THEN
			dw_palletfrutahisto.Object.pafh_secuen[ll_nueva3] = ll_nueva3
		ELSE	
			dw_palletfrutahisto.Object.pafh_secuen[ll_nueva3] = dw_6.Object.pafr_secuen[ll_fila3]    
		END IF	
		dw_palletfrutahisto.Object.pafh_ccajas[ll_nueva3] = dw_6.Object.pafr_ccajas[ll_fila3]    
		dw_palletfrutahisto.Object.pafh_nrlote[ll_nueva3] = dw_6.Object.pafr_nrlote[ll_fila3]    
		dw_palletfrutahisto.Object.pafr_fecing[ll_nueva3] = dw_6.Object.pafr_fecing[ll_fila3]    
		dw_palletfrutahisto.Object.pafr_fecemb[ll_nueva3] = dw_6.Object.pafr_fecemb[ll_fila3]    
		dw_palletfrutahisto.Object.pafr_copack[ll_nueva3] = dw_6.Object.pafr_copack[ll_fila3]    
		dw_palletfrutahisto.Object.pafr_tipdoc[ll_nueva3] = 5 
		dw_palletfrutahisto.Object.pafr_huert1[ll_nueva3] = dw_6.Object.pafr_huert1[ll_fila3]  
		dw_palletfrutahisto.Object.pafr_cuart1[ll_nueva3] = dw_6.Object.pafr_cuart1[ll_fila3] 
		dw_palletfrutahisto.Object.pafr_huert2[ll_nueva3] = dw_6.Object.pafr_huert2[ll_fila3]  
		dw_palletfrutahisto.Object.pafr_cuart2[ll_nueva3] = dw_6.Object.pafr_cuart2[ll_fila3] 
		dw_palletfrutahisto.Object.pafr_varrot[ll_nueva3] = dw_6.Object.pafr_varrot[ll_fila3]
		dw_palletfrutahisto.Object.pafr_huert4[ll_nueva3] = dw_6.Object.pafr_huert4[ll_fila3]  
		dw_palletfrutahisto.Object.pafr_cuart4[ll_nueva3] = dw_6.Object.pafr_cuart4[ll_fila3]
		dw_palletfrutahisto.Object.pafr_rotpak[ll_nueva3] = dw_6.Object.pafr_rotpak[ll_fila3]
		dw_palletfrutahisto.Object.pafr_calrot[ll_nueva3] = dw_6.Object.pafr_calrot[ll_fila3]
		dw_palletfrutahisto.Object.pafr_prdrot[ll_nueva3] = dw_6.Object.pafr_prdrot[ll_fila3]
		dw_palletfrutahisto.Object.pafr_docrel[ll_nueva3] = dw_6.Object.pafr_docrel[ll_fila3]
		dw_palletfrutahisto.Object.cate_codigo[ll_nueva3] = dw_6.Object.cate_codigo[ll_fila3]
		dw_palletfrutahisto.Object.pafr_catrot[ll_nueva3] = dw_6.Object.pafr_catrot[ll_fila3]
		
	NEXT
NEXT
	
IF dw_alpalletencab.Rowcount() > 0 THEN
	lb_AutoCommit		=	sqlca.AutoCommit
	sqlca.AutoCommit	=	False
	
	IF dw_alpalletencab.Update(True, False) = 1 THEN
		IF dw_alpalletfruta.Update(True, False) = 1 THEN
			IF dw_palletencahisto.Update(True, False) = 1 THEN
				IF dw_palletfrutahisto.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
				//		F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
						Return 
					ELSE
						lb_Retorno	=	True
						
						dw_alpalletencab.ResetUpdate()
						dw_alpalletfruta.ResetUpdate()
						dw_palletencahisto.ResetUpdate()
						dw_palletfrutahisto.ResetUpdate()
					END IF
					
				ELSE	
						F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
					Return 
				END IF	
			ELSE	
					F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
					Return 
			END IF		
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
			Return 
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
		
		Return 
	END IF
	
	Update dbo.despafrigoen Set
	defe_proces = :ll_numero
	where defe_numero = :ll_despacho
	and	clie_codigo = :li_cliente
	and	plde_codigo = :li_planta
	Using Sqlca;
	
	sqlca.AutoCommit	=	lb_AutoCommit
	
	Return 
END IF
								  


end event

public subroutine habilitaencab (boolean habilita);If Habilita THEN
	dw_2.Object.plde_codigo.Protect	= 0
	dw_2.Object.defe_numero.Protect	= 0
	dw_2.Object.defe_fecdes.Protect	= 0
	dw_2.Object.defe_horade.Protect	= 0
	
	dw_2.Object.defe_numero.Color	= 0
	dw_2.Object.plde_codigo.Color 	= 0
	dw_2.Object.defe_fecdes.Color 	= 0
	dw_2.Object.defe_horade.Color 	= 0
	
	dw_2.Object.defe_numero.BackGround.Color	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.defe_fecdes.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.defe_horade.BackGround.Color 	= Rgb(255,255,255)
	
	If dw_2.RowCount() > 0 Then
		If isnull(dw_2.Object.defe_plasag[1]) OR isnull(dw_2.Object.defe_plasag[1] = 0) Then
			dw_2.Object.defe_espmul.Protect					= 1
			dw_2.Object.defe_espmul.Color 					= Rgb(255,255,255)
			dw_2.Object.defe_espmul.BackGround.Color 	= 553648127
		Else
			dw_2.Object.defe_espmul.Protect					= 0
			dw_2.Object.defe_espmul.Color 					= 0
			dw_2.Object.defe_espmul.BackGround.Color	= Rgb(255,255,255)
		End If
	End If
	dw_2.SetColumn("defe_numero")
	dw_2.SetFocus()
Else
	dw_2.Object.defe_numero.Protect	= 1
	dw_2.Object.plde_codigo.Protect	= 1
	dw_2.Object.defe_fecdes.Protect	= 1
	dw_2.Object.defe_horade.Protect	= 1
	
	dw_2.Object.defe_numero.Color	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	= Rgb(255,255,255)
	dw_2.Object.defe_fecdes.Color 	= Rgb(255,255,255)
	dw_2.Object.defe_horade.Color 	= Rgb(255,255,255)
	
	dw_2.Object.defe_numero.BackGround.Color	= 553648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
	dw_2.Object.defe_fecdes.BackGround.Color 	= 553648127
	dw_2.Object.defe_horade.BackGround.Color 	= 553648127
	
	If isnull(dw_2.Object.defe_plasag[1]) OR isnull(dw_2.Object.defe_plasag[1] = 0) Then
		dw_2.Object.defe_espmul.Protect					= 1
		dw_2.Object.defe_espmul.Color					=	Rgb(255,255,255)
		dw_2.Object.defe_espmul.BackGround.Color	=	553648127
	Else
		dw_2.Object.defe_espmul.Protect					= 0
		dw_2.Object.defe_espmul.Color 					= 0
		dw_2.Object.defe_espmul.BackGround.Color 	= RGB(255,255,255)
	End If
End If
end subroutine

public subroutine habilitaingreso (string columna);Date		ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

If dw_2.RowCount() > 0 THEN
	If IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &		
		IsNull(dw_2.Object.defe_fecdes[1]) OR dw_2.Object.defe_fecdes[1] = ld_fecha OR &
		IsNull(dw_2.Object.defe_cancaj[1]) OR dw_2.Object.defe_cancaj[1] = 0 OR &
		IsNull(dw_2.Object.defe_cantar[1]) OR dw_2.Object.defe_cantar[1] = 0 OR &
		IsNull(dw_2.Object.defe_especi[1]) OR dw_2.Object.defe_especi[1] = 0 THEN
		lb_estado = False			
	END If
END If

If ii_blockcont = 1 THEN
	If IsNull(dw_2.Object.defe_nrcont[1]) OR dw_2.Object.defe_nrcont[1] = '' Then 
		lb_estado = False
   END If
END If
	
IF dw_2.Object.defe_plasag[1]  > 0 THEN

ELSE	
	lb_estado = True
END IF

IF dw_2.Object.tica_codigo[1] = 2 OR dw_2.Object.tica_codigo[1] = 3  THEN		
	IF IsNull(dw_2.Object.tran_codigo[1]) OR dw_2.Object.tran_codigo[1] = 0 OR &
		IsNull(dw_2.Object.defe_traser[1]) OR dw_2.Object.defe_traser[1] = 0 OR &
		IsNull(dw_2.Object.defe_chofer[1]) OR dw_2.Object.defe_chofer[1] = '' OR &
		IsNull(dw_2.Object.defe_chfrut[1]) OR dw_2.Object.defe_chfrut[1] = '' OR &
		IsNull(dw_2.Object.defe_patent[1]) OR dw_2.Object.defe_patent[1] = ''  &
	 THEN
		lb_estado = False
	END IF
END IF

IF dw_2.Object.tica_codigo[1] = 2 THEN
	IF IsNull(dw_2.Object.defe_nrcont[1]) OR dw_2.Object.defe_nrcont[1] = '' OR &		
		IsNull(dw_2.Object.tpco_codigo[1]) OR dw_2.Object.tpco_codigo[1] = 0  Then //OR &
		lb_estado = False
   END IF
END IF

IF IsNull(dw_2.Object.defe_especi[1]) OR dw_2.Object.defe_especi[1] = 0 THEN
	lb_estado = False
END IF

IF istr_mant.argumento[27] = '21' OR istr_mant.argumento[27] = '16'  THEN
	IF isnull(dw_2.Object.clpr_rut[1]) OR dw_2.Object.clpr_rut[1] = '' OR &
		isnull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] = '' THEN
		lb_estado = False
	END IF	
END IF

IF istr_mant.argumento[27] = '7' OR istr_mant.argumento[27] = '8' OR istr_mant.argumento[27] = '9' THEN
	IF isnull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] = '' THEN
		lb_estado = False
	END IF	
END IF	
			
pb_grabar.Enabled	=	lb_estado
pb_ins_det.Enabled  	= 	lb_estado
pb_eli_det.Enabled  	= 	lb_estado
pb_imprimir.Enabled 	= 	lb_estado
end subroutine

public subroutine buscaembarque ();Str_busqueda	lstr_busq

dw_2.Modify("buscaembarque.border = 0")
dw_2.Modify("buscaembarque.border = 5")

lstr_busq.argum[1]	=	istr_mant.argumento[3] // Cliente.

OpenWithParm(w_busc_embarques_consignatario, lstr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	dw_2.SetItem(il_fila, "embq_codigo", istr_busq.argum[1])
	dw_2.SetItem(il_fila, "embq_nomnav", istr_busq.argum[2])
	dw_2.SetItem(il_fila, "puer_codigo", Integer(istr_busq.argum[4]))
   	
	noexisteembarque(istr_busq.argum[1])
	
	istr_mant.argumento[7]	=	istr_busq.argum[5]
ELSE
	dw_2.SetColumn("embq_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscaembarque.border = 0")
dw_2.Modify("buscaembarque.border = 6")

end subroutine

public function boolean conexionbase ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta
Integer	li_Planta

DISCONNECT USING sqlconec;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

li_planta	=	dw_2.Object.defe_plades[1]

  SELECT pro.cone_nomodb,pro.cone_nomser,pro.cone_nombas,
         pro.cone_nodbms,pro.cone_nomusu,pro.cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dbo.plantadesp as pla,dbo.prodconectividad as pro  
   WHERE pla.plde_interp = pro.cone_codigo  and  
         pla.plde_codigo = :li_planta ;

sqlconec.ServerName	=	ls_nomser
sqlconec.DataBase	   =	ls_nombas
sqlconec.Dbms			= 	ls_nodbms
sqlconec.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlconec;

IF sqlconec.SQLCode = 0 THEN
	ib_Conectado	=	True
ELSE
	ib_Conectado	=	False
END IF

RETURN ib_Conectado

end function

public function long buscanuevofolio (integer planta);/* Busca Folio para hacer una recepción de pallet a partir de
   un despacho de interpanta
	usando conextividad*/

Integer	li_planta
Long		ll_numero
Boolean	lb_nulo

li_planta	=	planta

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM dbo.RECFRUPROCEE_TRANS
 WHERE plde_codigo = :li_planta
 USING sqlconec;

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla RecFruProcee_Trans")
ELSEIF sqlconec.SQLCode = 0 THEN
	
	 lb_nulo = IsNull(ll_numero)	
	
	 IF lb_nulo THEN
		 ll_numero=li_planta*10000
	 ELSE
		 ll_numero++
	 END IF
ELSE
	ll_numero=li_planta*10000
END IF

RETURN ll_numero
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

If Not dw_2.uf_check_required(0) Then RETURN False
If Not dw_1.uf_validate(0) Then RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If Borrando Then
	If dw_1.Update(True, False) = 1 Then
		If dw_2.Update(True, False) = 1 Then
			Commit;
			
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, This.Title)				
				RollBack;
			Else
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)			
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
Else
	If ids_CorrelMovim.Update()	=	1 Then
		If dw_2.Update(True, False) = 1 Then
			If dw_1.Update(True, False) = 1 Then
				If ids_palletfruta_fecha.Update()=1 Then		
					Commit;
					
					If sqlca.SQLCode <> 0 Then
						F_ErrorBaseDatos(sqlca, This.Title)						
						RollBack;
					Else
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						ids_palletfruta_fecha.ResetUpdate()
						ids_CorrelMovim.ResetUpdate()
					End If
				Else
					F_ErrorBaseDatos(sqlca, This.Title)				
					RollBack;				
				End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)				
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
End If

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean wf_actualiza_carga (boolean borrando);
Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlconec.AutoCommit
sqlconec.AutoCommit	=	False

IF dw_7.Update(True, False) = 1 THEN
	IF dw_8.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_11.Update(True, False) = 1 THEN
					IF dw_12.Update(True, False) = 1 THEN
						IF dw_15.Update(True, False)	=	1	THEN
							Commit;
								
							IF sqlconec.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlconec, This.Title)
								RollBack;
							ELSE
								lb_Retorno	=	True
														
								dw_7.ResetUpdate()
								dw_8.ResetUpdate()
								dw_3.ResetUpdate()
								dw_4.ResetUpdate()
								dw_11.ResetUpdate()
								dw_12.ResetUpdate()
								dw_15.ResetUpdate()										
							END IF
						ELSE
							F_ErrorBaseDatos(sqlconec, This.Title)
							RollBack;
						END IF
					ELSE
						F_ErrorBaseDatos(sqlconec, This.Title)
						RollBack;
					END IF
				
				ELSE
					F_ErrorBaseDatos(sqlconec, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlconec, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlconec, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlconec, This.Title)
		RollBack;
	END IF
ELSE
	F_ErrorBaseDatos(sqlconec, This.Title)
	RollBack;
END IF

sqlconec.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean busca_numeroinspeccion (integer cliente, integer planta, long pallet);Date ld_fecha
Long ll_num_inspec

il_numins = 0 ; ii_tipoin = 0 ; ii_secuen = 0 ;

SELECT Max(inpd_fechai) 
  INTO :ld_fecha
  FROM dbo.inspecpaldet 	
 WHERE clie_codigo = :Cliente AND  
		 plde_codigo = :Planta  AND  
		 paen_numero = :Pallet; 
			 
SELECT max(inpe_numero), inpe_tipoin, inpe_secuen 
  INTO :il_numins, :ii_tipoin, :ii_secuen  
  FROM dbo.inspecpaldet 	AS i 
 WHERE clie_codigo = :Cliente AND  
		 plde_codigo = :Planta  AND  
		 paen_numero = :Pallet  AND
		 inpd_fechai = :ld_fecha
GROUP BY inpe_tipoin, inpe_secuen		 ; 
			 
IF il_numins > 0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF


end function

public subroutine existe_cargaregistro ();Long ll_Filrec, ll_Filpal, ll_Numero, ll_Existe, ll_Pallet, ll_Filinp, ll_Numinp, ll_Secuen, ll_Totfil
Integer li_Planta, li_Cliente, li_Tipo

IF dw_2.RowCount() > 0 THEN
	
	li_Planta 	= dw_2.Object.defe_plades[1]
	ll_Numero 	= dw_2.Object.defe_numero[1]
	li_Cliente 	= dw_2.Object.clie_codigo[1]
	
	SELECT rfpe_numero  
	  INTO :ll_Existe  
	  FROM dbo.recfruprocee_trans 
	 WHERE plde_codigo = :li_Planta  AND  
			 clie_codigo = :li_Cliente AND  
			 rfpe_numero = :ll_Numero   
	USING sqlconec ;
	 
	IF ll_Existe > 0 THEN		
			DELETE FROM dbo.recfruproced_trans 
   		 WHERE plde_codigo = :li_Planta AND  
                rfpe_numero = :ll_Existe  AND  
                clie_codigo = :li_Cliente 
          USING sqlconec ;	
							
			DELETE FROM dbo.recfruprocee_trans  
			 WHERE plde_codigo = :li_Planta AND  
			       rfpe_numero = :ll_Existe  AND  
				    clie_codigo = :li_Cliente 
		   USING sqlconec ;
	END IF
 
  END IF
COMMIT ;

end subroutine

public subroutine generaarchivoplano ();Integer	li_Tabla, li_trans, li_especie, li_variedad, li_control
String	ls_Archivo,ls_Registro, ls_embalaje
Boolean	lb_Anulacion=True, lb_cambia = False
Long		ll_Fila, ll_FilPal, ll_Filadet, ll_Filinp, ll_Fildet, ll_filins, ll_destino

dw_11.SettransObject(sqlca)
dw_12.SettransObject(sqlca)
dw_11.Reset()
dw_12.Reset()
	
dw_13.Reset()

Open(w_mensaje)
w_mensaje.st_1.text = " Generando Archivo Plano"

IF	dw_2.RowCount() > 0 THEN
   ls_Archivo	=	String(dw_2.Object.defe_plades[1], '0000') + &
							String(dw_2.Object.defe_guides[1],'00000000') + &
							".Txt"
	li_Tabla		= 1	
	ls_Registro	=	String(li_Tabla)
	ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
	ls_Registro	+=	String(dw_2.Object.defe_numero[1], '00000000')
	ls_Registro	+=	String(dw_2.Object.clie_codigo[1], '000')
	ls_Registro += String(Today(), 'ddmmyyyy')
	ls_Registro	+=	String(2,'00')
	ls_Registro	+=	String(dw_2.Object.plde_codigo[1], '0000')
	ls_Registro	+=	String(dw_2.Object.defe_guides[1], '00000000')
	IF	IsNull(dw_2.Object.tran_codigo[1]) THEN
	 	ls_Registro += FILL('',4)
	ELSE
		ls_Registro	+=	String(dw_2.Object.tran_codigo[1], '0000')
	END IF
	 
	IF IsNull(dw_2.Object.tica_codigo[1]) THEN
		ls_Registro += FILL('0',2)
	ELSE
		ls_Registro	+=	String(dw_2.Object.tica_codigo[1], '00')
	END IF
	 
	IF IsNull(dw_2.Object.defe_patent[1]) THEN
		ls_Registro += FILL(' ', 20)
	ELSE
		ls_Registro	+=	String(dw_2.Object.defe_patent[1], Fill('@', 20))
	END IF
	 
	IF IsNull(dw_2.Object.defe_chofer[1]) THEN
		ls_Registro += FILL(' ', 50)
	ELSE
		ls_Registro	+=	String(dw_2.Object.defe_chofer[1], Fill('@', 50))
	END IF 
	IF IsNull(dw_2.Object.defe_fecact[1]) THEN
		ls_Registro 	+= Fill('0',8)
	ELSE
	 	ls_Registro 	+= String(dw_2.Object.defe_fecact[1], 'ddmmyyyy')
	END IF
	IF IsNull(dw_2.Object.defe_horact[1]) THEN
		ls_Registro 	+= Fill('0',6)
	ELSE
	   ls_Registro 	+= String(dw_2.Object.defe_horact[1], 'hhmmss')
	END IF
	
		dw_13.InsertRow(0)
	dw_13.Object.registro[1] = ls_Registro
	
	FOR ll_Fila = 1 TO dw_1.RowCount()
		ls_Registro	=	String(2) /*Recfruproced*/
		ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
		ls_Registro	+=	String(dw_1.Object.defe_numero[ll_Fila], '00000000')
		ls_Registro	+=	String(dw_1.Object.clie_codigo[ll_Fila], '000')
		ls_Registro	+=	String(dw_1.Object.paen_numero[ll_Fila], '00000000')
		  
		ll_filadet	=	dw_13.InsertRow(0)
		dw_13.Object.registro[ll_filadet]	=	ls_Registro
		  
		dw_5.GetChild("plde_codigo", dw_planta)
	   dw_planta.SetTransObject(sqlca)
	   dw_planta.Retrieve(1)
		  
		IF dw_5.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_Fila],dw_2.Object.plde_codigo[1]) > 0 THEN
			
	 	  	ls_Registro	=	String(3) /*PalletEncab*/	
		  	ls_Registro	+=	String(dw_5.Object.clie_codigo[1], '000')
		  	ls_Registro	+=	String(dw_5.Object.paen_numero[1], '00000000')
		  	ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
		  	IF IsNull(dw_5.Object.paen_tipopa[1]) THEN
		  		ls_Registro += FILL('0',1)
	     	ELSE
	        	ls_Registro	+=	String(dw_5.Object.paen_tipopa[1], '0')
	     	END IF
		  	IF IsNull(dw_5.Object.tpem_codigo[1]) THEN
		   	ls_Registro += FILL(' ',5)
		  	ELSE
				ls_Registro += FILL(' ',5)//ls_Registro	+=	String(dw_5.Object.tpem_codigo[1],  Fill('@', 5))
		  	END IF
		  	IF IsNull(dw_5.Object.espe_codigo[1]) THEN
				ls_Registro += FILL('0',2)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.espe_codigo[1], '00')
		  	END IF
		  	IF IsNull(dw_5.Object.vari_codigo[1]) THEN
				ls_Registro += FILL('0',4)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.vari_codigo[1], '0000')
		  	END IF
		  	IF IsNull(dw_5.Object.tiem_codigo[1]) THEN
				ls_Registro += FILL('  ',2)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.tiem_codigo[1], '00')
		  	END IF
		  	IF IsNull(dw_5.Object.cate_codigo[1]) THEN
			  	ls_Registro += FILL('0',3)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.cate_codigo[1], '000')
		  	END IF
		  	IF IsNull(dw_5.Object.etiq_codigo[1]) THEN
				ls_Registro += FILL('0',4)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.etiq_codigo[1], '0000')
		  	END IF
		  	IF IsNull(dw_5.Object.stat_codigo[1]) THEN
				ls_Registro += FILL('0',3)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.stat_codigo[1], '000')
		  	END IF
		  	IF IsNull(dw_5.Object.trat_codigo[1]) THEN
				ls_Registro += FILL('0',2)
			ELSE
		 		ls_Registro	+=	String(dw_5.Object.trat_codigo[1], '00')
		  	END IF
			IF IsNull(dw_5.Object.frio_codigo[1]) THEN
				ls_Registro += FILL(' ',1)
			ELSE
		  		ls_Registro	+=	String(dw_5.Object.frio_codigo[1],  Fill('@', 1))
		  	END IF
			IF IsNull(dw_5.Object.cond_codigo[1]) THEN
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.cond_codigo[1], '0')
			END IF
		   IF IsNull(dw_5.Object.dest_codigo[1]) THEN
				ls_Registro += FILL('0',3)
			ELSE
			  	ls_Registro	+=	String(dw_5.Object.dest_codigo[1], '000')
			END IF
			IF IsNull(dw_5.Object.emba_codigo[1]) THEN
				ls_Registro += FILL(' ',10)
			ELSE
				ls_Registro	+=	String(dw_5.Object.emba_codigo[1], Fill('@', 10))
			END IF
			IF IsNull(dw_5.Object.paen_fecemb[1]) THEN
				ls_Registro += FILL('0',8)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_fecemb[1], 'ddmmyyyy')
			END IF  
			IF IsNull(dw_5.Object.paen_cosecha[1]) THEN
				ls_Registro += FILL('0',8)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_cosecha[1], 'ddmmyyyy')
		   END IF
			IF IsNull(dw_5.Object.paen_altura[1]) THEN
				ls_Registro += FILL('0',4)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_altura[1], '0000')
		   END IF 
			IF IsNull(dw_5.Object.paen_ccajas[1]) THEN
				ls_Registro += FILL('0',7)
			ELSE
			  	ls_Registro	+=	String(dw_5.Object.paen_ccajas[1], '0000000')
			END IF
			IF IsNull(dw_5.Object.tmvp_codigo[1]) THEN
				ls_Registro += FILL('0',2)
			ELSE
			  ls_Registro	+=	String(dw_5.Object.tmvp_codigo[1], '00')
		   END IF
			IF	IsNull(dw_5.Object.paen_fecini[1]) THEN
				ls_Registro += FILL('00000000',8)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_fecini[1],  'ddmmyyyy')
		   END IF
			IF IsNull(dw_5.Object.paen_horain[1]) THEN
				ls_Registro += FILL('0',6)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_horain[1], 'hhmmss')
		   END IF
			ls_Registro += FILL('0',4) 	//Camara
			ls_Registro += FILL('0',2) 	//Calle
			ls_Registro += FILL('0',2) 	//Base
			ls_Registro += FILL('0',1) 	//Posición
			ls_Registro	+=	String(1, '0') //Estado
		   
			IF IsNull(dw_5.Object.paen_inspec[1]) THEN 
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_inspec[1], '0')
		   END IF
			IF IsNull(dw_5.Object.paen_concal[1]) THEN  
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_concal[1], '0')
		   END IF
			IF IsNull(dw_5.Object.paen_pexpor[1]) THEN  
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_pexpor[1], '0')
		   END IF
			IF IsNull(dw_5.Object.paen_pmixto[1]) THEN 
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_pmixto[1], '0')
		   END IF
			IF IsNull(dw_5.Object.paen_varrot[1]) THEN
				ls_Registro += FILL('0',4)
			ELSE			  
				ls_Registro	+=	String(dw_5.Object.paen_varrot[1], '0000')
		   END IF
			IF IsNull(dw_5.Object.paen_nrasda[1]) THEN
				ls_Registro += FILL(' ',16)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_nrasda[1] ,Fill('@', 16))
		   END IF
			IF NOT IsNull(dw_5.Object.copa_codigo[1]) THEN
				ls_Registro	+=	String(dw_5.Object.copa_codigo[1], '000')
		   END IF
			  
			ll_filadet	=	dw_13.InsertRow(0)
		   dw_13.Object.registro[ll_filadet]	=	ls_Registro
			
			ll_destino = dw_2.Object.defe_plades[1]
			  
			SELECT isnull(plde_cajbul,0) 
			INTO :li_control  
			FROM dbo.plantadesp
			WHERE plde_codigo = :ll_destino;
			
			IF	dw_6.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.Object.plde_codigo[1],li_control) > 0 THEN
			  				
	 			FOR ll_FilPal = 1 TO dw_6.RowCount()
			 		ls_Registro	=	String(4) /*PalletFruta*/	
					ls_Registro	+=	String(dw_6.Object.clie_codigo[ll_FilPal], '000')
					ls_Registro	+=	String(dw_6.Object.paen_numero[ll_FilPal], '00000000')
					ls_Registro	+=	String(dw_6.Object.espe_codigo[ll_FilPal], '00')
					ls_Registro	+=	String(dw_6.Object.vari_codigo[ll_FilPal], '0000')
					ls_Registro	+=	String(dw_6.Object.emba_codigo[ll_FilPal] ,Fill('@', 10))
					ls_Registro	+=	String(dw_6.Object.prod_codigo[ll_FilPal], '00000')
					ls_Registro	+=	String(dw_6.Object.cond_codigo[ll_FilPal], '0')
					ls_Registro	+=	String(dw_6.Object.etiq_codigo[ll_FilPal], '0000')
					ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
					ls_Registro	+=	String(dw_6.Object.pafr_calibr[ll_FilPal] ,Fill('@', 3))
					
					IF li_control = 0 THEN
						ls_Registro	+=	String(ll_FilPal, '00000000')
					ELSE	
						ls_Registro	+=	String(dw_6.Object.pafr_secuen[ll_FilPal], '00000000')
					END IF
					
					IF IsNull(dw_6.Object.pafr_ccajas[ll_FilPal]) THEN
						ls_Registro += FILL('0',7)
					ELSE
						ls_Registro	+=	String(dw_6.Object.pafr_ccajas[ll_FilPal], '0000000')
					END IF
					IF IsNull(dw_6.Object.pafr_nrlote[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSE
						ls_Registro	+=	String(dw_6.Object.pafr_nrlote[ll_FilPal], '0000')
					END IF
					IF IsNull(dw_6.Object.pafr_copack[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSEIF dw_6.Object.pafr_copack[ll_FilPal] = -1 THEN
						ls_Registro	+=	String(dw_6.Object.pafr_copack[ll_FilPal], '0000')
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_copack[ll_FilPal], '0000')
					END IF
					IF IsNull(dw_6.Object.pafr_varrot[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_varrot[ll_FilPal], '0000')
					END IF
					IF IsNull(dw_6.Object.pafr_prdrot[ll_FilPal]) THEN
						ls_Registro += FILL('0',5)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_prdrot[ll_FilPal], '00000')
					END IF
					IF IsNull(dw_6.Object.pafr_calrot[ll_FilPal]) THEN
						ls_Registro += FILL(' ',3)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_calrot[ll_FilPal],  Fill('@', 3))
					END IF
					IF IsNull(dw_6.Object.pafr_huert1[ll_FilPal]) THEN
						ls_Registro += FILL('0',5)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_huert1[ll_FilPal], '00000')
					END IF
					IF IsNull(dw_6.Object.pafr_cuart1[ll_FilPal]) THEN
						ls_Registro += FILL('0',5)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_cuart1[ll_FilPal], '00000')
					END IF
					
					IF IsNull(dw_6.Object.pafr_fecemb[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_fecemb[ll_FilPal], 'ddmmyyyy')
					END IF
					IF IsNull(dw_6.Object.pafr_fecing[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_fecing[ll_FilPal], 'ddmmyyyy')
					END IF					
					IF IsNull(dw_6.Object.pafr_huert4[ll_FilPal]) THEN
						ls_Registro += FILL('0',5)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_huert4[ll_FilPal], '00000')
					END IF
					IF IsNull(dw_6.Object.pafr_cuart4[ll_FilPal]) THEN
						ls_Registro += FILL('0',5)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_cuart4[ll_FilPal], '00000')
					END IF		
					IF IsNull(dw_6.Object.pafr_rotpak[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_rotpak[ll_FilPal], '0000')
					END IF					
					
					ll_filadet	=	dw_13.InsertRow(0)
		         dw_13.Object.registro[ll_filadet]	=	ls_Registro
				NEXT
			END IF
			
			IF dw_14.Retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[ll_fila]) > 0 THEN	
				FOR ll_FilPal = 1 TO dw_14.RowCount()
			 		ls_Registro	=	String(7) /*Spro_Cajasprod*/	
					ls_Registro	+=	String(dw_14.Object.clie_codigo[ll_FilPal], '000')	
					ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
					ls_Registro	+=	String(dw_14.Object.capr_numero[ll_FilPal], '00000000')
					ls_Registro	+=	String(dw_14.Object.espe_codigo[ll_FilPal], '00')
					ls_Registro	+=	String(dw_14.Object.vari_codigo[ll_FilPal], '0000')
					ls_Registro	+=	String(dw_14.Object.prod_codigo[ll_FilPal], '00000')	
					IF IsNull(dw_14.Object.prod_predio[ll_FilPal]) THEN
						ls_Registro	+=	'000'							
					ELSE
						ls_Registro	+=	String(dw_14.Object.prod_predio[ll_FilPal], '000')	
					END IF
					
					IF IsNull(dw_14.Object.prod_huerto[ll_FilPal]) THEN
						ls_Registro	+=	'000'					
					ELSE
	 					ls_Registro	+=	String(dw_14.Object.prod_huerto[ll_FilPal], '000')	
					END IF
					
					IF IsNull(dw_14.Object.prod_cuarte[ll_FilPal]) THEN
						ls_Registro	+=	'000'					
					ELSE
						ls_Registro	+=	String(dw_14.Object.prod_cuarte[ll_FilPal], '000')						 
					END IF
					
					ls_Registro	+=	String(dw_14.Object.emba_codigo[ll_FilPal] ,Fill('@', 10))	
					IF IsNull(dw_14.Object.etiq_codigo[ll_FilPal]) THEN
						ls_Registro	+=	'000'					
					ELSE
						ls_Registro	+=	String(dw_14.Object.etiq_codigo[ll_FilPal], '000')	
					END IF
					IF IsNull(dw_14.Object.capr_fecemb[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)
					ELSE
					   ls_Registro	+=	String(dw_14.Object.capr_fecemb[ll_FilPal], 'ddmmyyyy')
					END IF
					ls_Registro	+=	String(dw_14.Object.capr_calibr[ll_FilPal] ,Fill('@', 3))					
					IF IsNull(dw_14.Object.capr_embala[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)						
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_embala[ll_FilPal], '00000000')
					END IF
					IF IsNull(dw_14.Object.capr_selecc[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)						
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_selecc[ll_FilPal], '00000000')
					END IF
					IF IsNull(dw_14.Object.capr_pesado[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)						
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_pesado[ll_FilPal], '00000000')
					END IF
					IF IsNull(dw_14.Object.capr_cean14[ll_FilPal]) THEN
						ls_Registro += FILL(' ',14)						
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_cean14[ll_FilPal] ,Fill('@', 14))	
					END IF
					ls_Registro	+=	String(dw_14.Object.capr_numpal[ll_FilPal], '00000000')
					ls_Registro	+=	String(dw_14.Object.capr_regcap[ll_FilPal] ,Fill('@', 100))	
					ls_Registro	+=	String(dw_14.Object.capr_estado[ll_FilPal], '0')			
					IF IsNull(dw_14.Object.capr_varrot[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)									
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_varrot[ll_FilPal], '0000')	
					END IF
					IF IsNull(dw_14.Object.capr_numgia[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)									
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_numgia[ll_FilPal], '00000000')	
					END IF
					ls_Registro	+=	String(dw_14.Object.cate_codigo[ll_FilPal], '000')					
					IF IsNull(dw_14.Object.capr_cespak[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)															
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_cespak[ll_FilPal], '0000')	
					END IF
					ls_Registro	+=	String(dw_14.Object.capr_docrel[ll_FilPal], '00000000')	
					IF IsNull(dw_14.Object.capr_hordig[ll_FilPal]) THEN
						ls_Registro += FILL('0',6)
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_hordig[ll_FilPal], 'hhmmss')
					END IF
					
					IF IsNull(dw_14.Object.capr_fecdig[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)
					ELSE
					   ls_Registro	+=	String(dw_14.Object.capr_fecdig[ll_FilPal], 'ddmmyyyy')
					END IF
					
					IF IsNull(dw_14.Object.capr_nrlote[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_nrlote[ll_FilPal], '0000')
					END IF				
										 
					ll_filadet	=	dw_13.InsertRow(0)
		         dw_13.Object.registro[ll_filadet]	=	ls_Registro
				NEXT
			END IF
		END IF
		
		IF dw_1.Object.paen_inspec[ll_fila] = 1 THEN
			IF Busca_Numeroinspeccion(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[ll_fila]) THEN
				carga_inspeccion(dw_1.Object.paen_numero[ll_fila],ll_fila,dw_5.Object.dest_codigo[1])
			END IF
		END IF
	NEXT	
	
	IF dw_11.RowCount() > 0 THEN
		FOR ll_Filinp = 1 TO dw_11.RowCount()
			ls_Registro	=	String(5) /*Inspecpalenc*/	
			ls_Registro	+=	String(dw_11.Object.inpe_tipoin[ll_Filinp], '0')
			ls_Registro	+=	String(dw_11.Object.inpe_numero[ll_Filinp], '00000000')
			ls_Registro	+=	String(dw_11.Object.clie_codigo[ll_Filinp], '000')
			ls_Registro	+=	String(dw_11.Object.plde_codigo[ll_Filinp], '0000')
			ls_Registro	+=	String(dw_11.Object.inpe_secuen[ll_Filinp], '00')
			ls_Registro	+=	String(dw_11.Object.dest_codigo[ll_Filinp], '000')
			ls_Registro	+=	String(dw_11.Object.inpe_fechai[ll_Filinp], 'ddmmyyyy')
						
			SELECT empr_especi,empr_varied,empr_embala
			INTO :li_especie,:li_variedad,:ls_embalaje
			FROM dbo.parempresa;
			
			IF IsNull(dw_11.Object.espe_codigo[ll_Filinp]) THEN
				ls_Registro += String(li_especie,'00')
			ELSE
				ls_Registro	+=	String(dw_11.Object.espe_codigo[ll_Filinp], '00')
			END IF
			IF IsNull(dw_11.Object.vari_codigo[ll_Filinp]) THEN
			   ls_Registro += string(li_variedad,'0000') 
			ELSE
				ls_Registro	+=	String(dw_11.Object.vari_codigo[ll_Filinp], '0000')
			END IF
			IF IsNull(dw_11.Object.emba_codigo[ll_Filinp]) THEN
			   ls_Registro += string(ls_embalaje,Fill('@', 10))
			ELSE
				ls_Registro	+=	String(dw_11.Object.emba_codigo[ll_Filinp], Fill('@', 10))
			END IF
			IF IsNull(dw_11.Object.tpem_codigo[ll_Filinp]) THEN
				ls_Registro += String(dw_5.Object.tpem_codigo[1], Fill('@', 5))
			ELSE
				ls_Registro	+=	String(dw_11.Object.tpem_codigo[ll_Filinp], Fill('@', 5))
			END IF
			IF IsNull(dw_11.Object.inpe_todpal[ll_Filinp]) THEN
				ls_Registro += FILL('0',1) 
			ELSE
				ls_Registro	+=	String(dw_11.Object.inpe_todpal[ll_Filinp], '0')
			END IF
			IF IsNull(dw_11.Object.inpe_calibr[ll_Filinp]) THEN
				ls_Registro += FILL(' ',3)
			ELSE
				ls_Registro	+=	String(dw_11.Object.inpe_calibr[ll_Filinp], Fill('@', 3))
			END IF
			
			ll_filadet	=	dw_13.InsertRow(0)
		   dw_13.Object.registro[ll_filadet]	=	ls_Registro
		NEXT
	END IF
	IF dw_12.RowCount() > 0 THEN
		FOR ll_Fildet = 1 TO dw_12.RowCount()
			ls_Registro	=	String(6) /*Inspecpaldet*/	
			ls_Registro	+=	String(dw_12.Object.inpe_tipoin[ll_Fildet], '0')
			ls_Registro	+=	String(dw_12.Object.inpe_numero[ll_Fildet], '00000000')
			ls_Registro	+=	String(dw_12.Object.clie_codigo[ll_Fildet], '000')
			ls_Registro	+=	String(dw_12.Object.plde_codigo[ll_Fildet], '0000')
			ls_Registro	+=	String(dw_12.Object.inpe_secuen[ll_Fildet], '00')
			ls_Registro	+=	String(dw_12.Object.paen_numero[ll_Fildet], '00000000')
			IF IsNull(dw_12.Object.dest_codigo[ll_Fildet]) THEN
				ls_Registro += FILL('0',3) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.dest_codigo[ll_Fildet], '000')
			END IF
			IF IsNull(dw_12.Object.inpd_fechai[ll_Fildet]) THEN
				ls_Registro += FILL('0',8) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.inpd_fechai[ll_Fildet], 'ddmmyyyy')
			END IF
			IF IsNull(dw_12.Object.inpd_nroanu[ll_Fildet]) THEN
				ls_Registro += FILL('0',8) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.inpd_nroanu[ll_Fildet], '00000000')
			END IF
			IF IsNull(dw_12.Object.inpd_fechaa[ll_Fildet]) THEN
				ls_Registro += FILL('0',8) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.inpd_fechaa[ll_Fildet], 'ddmmyyyy')
			END IF
			IF IsNull(dw_12.Object.inpd_frecha[ll_Fildet]) THEN
				ls_Registro += FILL('0',8) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.inpd_frecha[ll_Fildet], 'ddmmyyyy')
			END IF
	
	      ll_filadet	=	dw_13.InsertRow(0)
		   dw_13.Object.registro[ll_filadet]	=	ls_Registro
		NEXT	
	END IF
END IF

Close(w_mensaje)

IF dw_13.SaveAs(gs_disco+":\GeneradosInterplanta\" + ls_Archivo, Text!, False) = -1 THEN
	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
ELSE
	MessageBox("Atención","Archivo Generado en directorio GeneradosInterplanta " + ls_Archivo)
END IF
end subroutine

public function long busnuevofoliodespa (integer ai_cliente, integer ai_planta);/* Busca Folio para hacer un  Despacho de pallet */

Integer	li_Planta
Long		ll_Numero, ll_Inicia, ll_Termin, ll_Actual, ll_Quedan, ll_despacho
Boolean	lb_Nulo

li_Planta	=	ai_planta
ll_Numero	=	0

select max(defe_numero)
into :ll_despacho
from dbo.despafrigoen
where plde_codigo = :li_planta;
		
ids_CorrelMovim.Retrieve(li_Planta,2)
IF ids_CorrelMovim.RowCount() > 0 THEN
	ll_Inicia	=	ids_CorrelMovim.Object.como_inicia[1]
	ll_Termin	=	ids_CorrelMovim.Object.como_termin[1]
	ll_Actual	=	ids_CorrelMovim.Object.como_actual[1]
	
	IF Isnull(ll_Inicia) THEN ll_Inicia	=	0
	IF Isnull(ll_Termin) THEN ll_Termin	=	0
	IF Isnull(ll_Actual) THEN ll_Actual	=	0
		
	IF Isnull(ll_despacho) OR String(ll_despacho) = '' OR ll_despacho < ll_Inicia THEN
		ll_Actual = ll_Inicia
	ELSE
		ll_Actual=	ll_despacho
	END IF	

	IF ll_Inicia >= 0 AND ll_Termin > 0 THEN
		IF ll_Actual	=	0	THEN
			ll_Actual	=	ll_Inicia + 1
		ELSE
			ll_Actual++		
		END IF
		
		IF ll_Actual >= ll_Inicia AND	ll_Actual <= ll_Termin	THEN
		
			IF ll_Actual > ll_Termin THEN
				Messagebox("Atención","No Existen Números de Folios Disponibles de Movimiento de Despacho",exclamation!) 			
				ll_Numero	=	0
				RETURN ll_Numero
			ELSEIF ll_Actual = ll_Termin THEN
				Messagebox("Atención","Ultimo Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
			END IF
			
			ll_Numero	=	ll_Actual
				
			ll_Quedan	=	(ll_Termin - ll_Actual)
			IF ll_Quedan <= 3 THEN
				Messagebox("Atención","Existen "+String(ll_Quedan)+" Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
			END IF
			ids_CorrelMovim.Object.como_actual[1]	=	ll_Actual
		ELSE
			Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
			ll_Numero	=	0
			RETURN ll_Numero
		END IF		
	ELSE
		Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
		ll_Numero	=	0
		RETURN ll_Numero	
	END IF
ELSE
	Messagebox("Atención","No Existe Ningún Número de Movimiento de Despacho",exclamation!) 
	ll_Numero	=	0
	RETURN ll_Numero	
END IF

RETURN ll_numero
end function

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente
Long		ll_nfolio

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.defe_numero[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "defe_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_existe
	FROM	dbo.DESPAFRIGOEN
	WHERE	plde_codigo	=	:li_planta
	AND	defe_numero	=	:ll_nfolio 
	And   clie_codigo =  :li_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Despafrigoen")
	RETURN False
ELSEIF li_existe > 0 THEN
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	
	This.TriggerEvent("ue_recuperadatos")
	IF il_cierra = 1 THEN
		Return False
	END IF
	istr_mant.argumento[3]	= 	String(dw_2.Object.clie_codigo[1])
	istr_mant.argumento[4]	= 	String(dw_2.Object.defe_cantar[1])
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
	ib_existe_folioD			=	True
   RETURN False
ELSE
	IF IsNull(ll_nfolio) THEN
		istr_mant.argumento[1]	= String(li_planta)
		istr_mant.argumento[2]	= String(ll_nfolio)
		//istr_mant.argumento[3]	= String(li_cliente)
		ib_existe_folioD			=	False
		RETURN False
	ELSE
	   MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
		ib_existe_folioD			=	False
	   RETURN True
	END IF	
END IF





end function

public function boolean buscaregistros ();Integer  li_existe, li_planta, li_cliente
Long		ll_nfolio

//li_planta = dw_2.Object.plde_codigo[1]
li_planta = dw_2.Object.defe_plades[1]
li_cliente = dw_2.Object.clie_codigo[1]
ll_nfolio  = dw_2.Object.defe_numero[1]

SELECT	Count(*)
	INTO	:ii_existe
	FROM	dbo.RECFRUPROCEE_TRANS
	WHERE	plde_codigo	=	:li_planta
	AND	rfpe_numero	=	:ll_nfolio 
	Using (sqlconec);
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RECFRUPROCEE_TRANS")
	RETURN True
END IF	
	
IF ii_existe > 0 THEN	
	
	ll_nfolio  = dw_2.Object.defe_guides[1]
	
	SELECT	Count(*)
		INTO	:ii_existe
		FROM	dbo.RECFRUPROCEE_TRANS
		WHERE	plde_codigo	=	:li_planta
		AND	rfpe_numero	=	:ll_nfolio 
		Using (sqlconec);
		
	IF ii_existe = 0 THEN
		li_existe = 0 
		ii_existe = 1
	ELSE
		li_existe = 1
	END IF	
END IF

IF li_existe > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF	

RETURN False


end function

public function boolean coneccionbase ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

DISCONNECT USING sqlconec2;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

  SELECT cone_nomodb,cone_nomser,cone_nombas,
         cone_nodbms,cone_nomusu,cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dbo.prodconectividad   
   WHERE cone_codigo = 90;

sqlconec2.ServerName	=	ls_nomser
sqlconec2.DataBase	   =	ls_nombas
sqlconec2.Dbms			= 	ls_nodbms
sqlconec2.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlconec2;

IF sqlconec2.SQLCode = 0 THEN
	ib_Conectado2	=	True
ELSE
	ib_Conectado2	=	False
END IF

RETURN ib_Conectado2

end function

public subroutine enviamail ();String			ls_DirectorioAct, ls_rut, ls_Contenedor, ls_Archivo,  ls_embarques, ls_ruta,ls_Archivo1,ls_Archivo2,ls_Archivo3
Long			ll_Fila, ll_Archivo, ll_guia
Boolean		lb_Existe
str_parms	lstr_parms

If dw_ArchPlano.Retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.defe_numero[1]) < 1 Then
	Messagebox("Error", "Los pallets involucrados en este despacho no poseen detalle de cajas~n~r" + &
								"No se pudo concretar envio Automático de E-Mail", Exclamation!)
Else
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	
	ls_Archivo	= '\Interplanta' + String(dw_2.Object.plde_codigo[1], '00000') + String(dw_2.Object.defe_numero[1], '00000000') + '.CSV'
	
	If dw_archplano.SaveAs(ls_ruta + ls_archivo, CSV!, FALSE) = -1 Then
		MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especIficada~n~r" + &
						ls_ruta + ls_archivo+"~n~rNo se pudo concretar envio Automático de E-Mail", StopSign!)
		Return
	Else
		Long		ll_FilaDet, ll_destinoins, ll_Numero, ll_Filas, ll_Filas2, ll_nroguia
		Integer	li_PldSag, li_cliente, li_planta
		String		ls_Registro
		
		dw_inspeccion_deta.reset()
		dw_inspeccion_enc.reset()
		
		dw_inspeccion_enc.SetTransObject(Sqlca)
		dw_inspeccion_deta.SetTransObject(Sqlca)
		
		dw_inspeccion_enc.Reset()
		
		ll_Numero = Long(dw_2.Object.defe_numero[1])
		li_cliente	= Long(dw_2.Object.clie_codigo[1])
		li_planta	= Long(dw_2.Object.plde_codigo[1])
		
		ll_Filas	= dw_inspeccion_enc.Retrieve(Integer(li_cliente), Integer(li_planta), ll_Numero)
		ll_Filas2	= dw_inspeccion_deta.Retrieve(Integer(li_cliente), Integer(li_planta), ll_Numero)
		
		If ll_Filas = -1 Then
			F_ErrorBaseDatos(sqlca,"Recuperación datos de Planilla S.A.G.")
		
		ElseIf ll_Filas = 0 Then
			MessageBox("Atención", "No hay información para Planilla Indicada.~r~rIngrese otro Número.", Exclamation!, Ok!)
			pb_grabar.Enabled	= False
		
		Else
			If dw_inspeccion_enc.retrieve(Integer(li_cliente), Integer(li_planta), ll_Numero) < 1 Then
				Messagebox("Error", "Despacho NO involucra Inspecciones~n~rNo se pudo Generar la Inspecpalenc", Exclamation!)
			Else
				ls_Archivo1	= '\Inspecpalenc' + String(ll_Numero, '00000000') + '.CSV'
				If dw_inspeccion_enc.SaveAs(ls_ruta + ls_archivo1, CSV!	 ,FALSE) = -1 Then
					MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especIficada~n~r" + &
									ls_ruta + ls_archivo1+"~n~r", StopSign!)
					Return
				End If	
			End If
			
				If dw_inspeccion_deta.retrieve(Integer(li_cliente), Integer(li_planta), ll_Numero) < 1 Then
					Messagebox("Error", "Despacho NO involucra Detalle Inspecciones~n~r" + &
												"No se pudo Generar la Inspecpaldet", Exclamation!)
				Else
					ls_Archivo2	= '\Inspecpaldet' + String(ll_Numero, '00000000') + '.CSV'
					If dw_inspeccion_deta.SaveAs(ls_ruta + ls_archivo2, CSV!	 ,FALSE) = -1 Then
						MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especIficada~n~r" + &
										ls_ruta + ls_archivo2+"~n~r", StopSign!)
						Return		
					End If	
				End If
				
			dw_inspeccion_deta.Reset()
			dw_inspeccion_enc.Reset()
		
		End If
		
		ll_nroguia = dw_2.Object.defe_guides[1]
		ids_archivo	=	Create DataStore
		ls_Archivo3	=	"\Guia Despacho-"+String(ll_nroguia)+".xls"
		ids_archivo.DataObject	=	'dw_info_anexo_resumen_guia_despacho'
		ids_archivo.SetTransObject(sqlca)
		ids_archivo.Retrieve(li_cliente,li_planta,ll_nroguia)
		ids_archivo.SaveAs(ls_Ruta + ls_Archivo3,excel5!, True)
				
		ChangeDirectory ( ls_ruta )
		
		lstr_parms.string_arg[1]		=	Right(ls_Archivo, Len(ls_Archivo) - 1)
		lstr_parms.string_arg[2]		=	String(dw_2.Object.defe_numero[1])
		lstr_parms.string_arg[3]		=  RescataCorreo(dw_2.Object.defe_plades[1])
		lstr_parms.string_arg[4]		=	ls_ruta + ls_Archivo
		If ls_Archivo1 <> '' Then
			lstr_parms.string_arg[5]		=	ls_ruta + ls_Archivo1
			lstr_parms.string_arg[6]		=	ls_ruta + ls_Archivo2
		End If
		If ls_Archivo3 <> '' Then
			lstr_parms.string_arg[7]		=	ls_ruta + ls_Archivo3
		End If	
				
		OpenWithParm(w_correo_zonas, lstr_parms)
		
		ChangeDirectory ( ls_ruta )
	End If
End If
end subroutine

public function string rescatacorreo (integer ai_planta);String ls_correo
 
SELECT plde_correo 
 INTO :ls_correo  
 FROM dbo.plantadesp 
WHERE plde_codigo = :ai_planta
USING sqlca;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas")
ELSEIF sqlca.SQLCode = 100 THEN
	//RETURN True
ELSE
	RETURN ls_correo
END IF
end function

public function boolean buscasellos (long sello);Long		ll_numero, ll_desde, ll_hasta, ll_Null, ll_posici, ll_largosello, ll_sello
Boolean	lb_retorno=True
Integer	li_Planta, li_cansel, li_encuentra=0, li_haya
String	ls_Sellos, ls_Null

SetNull(ls_Null)
SetNull(ll_Null)

li_Planta	=	dw_2.Object.plde_codigo[1]
ls_Sellos	=	dw_2.Object.defe_numsel[1]
li_cansel	=	dw_2.Object.defe_cansel[1]

IF IsNull(ls_Sellos) THEN
	ls_Sellos=''
END IF

IF IsNull(li_cansel) THEN
	li_cansel=0
END IF

IF li_cansel > 0 THEN
	
	ll_largosello	=	len(ls_Sellos)
	ll_sello			=	len(String(sello))
	ll_sello			=	ll_sello+4
	
	For ll_posici = 1 To ll_largosello
		
		 IF Mid(ls_Sellos,ll_posici,1)='-' THEN li_encuentra++
	  	 IF Mid(ls_Sellos,ll_posici,ll_sello)='- '+String(sello)+' -' THEN li_haya++
			
	Next
	
	IF li_encuentra >= li_cansel THEN
			MessageBox("Error","Cantidad de Sellos Registrados Supera los Informados",Information!, Ok!)
			RETURN False
	END IF
	IF li_haya > 0 THEN
			MessageBox("Error","Número de Sello Duplicado",Information!, Ok!)
			RETURN False
	END IF

END IF

SELECT sell_inicio, sell_termin
  INTO :ll_desde, :ll_hasta
  FROM dbo.correlsellos
  WHERE plde_codigo = :li_planta
  AND   sell_vigenc = 0;

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla Correlsellos")
	lb_retorno = False
ELSEIF sqlconec.SQLCode = 0 THEN
	
	 IF sello < ll_desde OR sello > ll_hasta THEN
			MessageBox("Error","Sello Fuera de Rango de Control",Information!, Ok!)
			lb_retorno = False
	 ELSE
		 ls_Sellos	=	ls_Sellos + " - " + String(sello)
		 dw_2.Object.defe_numsel[1]	=	ls_Sellos
		 dw_2.Object.sello[1]			=  ll_Null
					 
		 lb_retorno = True
	 END IF

ELSE
	lb_retorno = False
END IF

RETURN lb_retorno
end function

protected function boolean noexisteembarque (string as_embarque);String	ls_Nombre, ls_recibidor
Integer	li_Puerto, li_Cliente, li_Destino
Long 		li_recibidor, li_Planta

li_Cliente	=	Integer(istr_mant.Argumento[3])
li_Planta = Integer(istr_mant.argumento[1])

SELECT	em.embq_nomnav, em.puer_codigo, em.dest_codigo, re.cons_nombre, em.embq_clifac
	INTO	:ls_Nombre, :li_Puerto, :li_Destino, :ls_recibidor, :li_recibidor
	FROM	dbo.embarqueprod as em, dbo.consignatario as re
	WHERE	em.clie_codigo	=	:li_Cliente
	AND	em.embq_codigo	=	:as_Embarque
   AND   re.cons_codigo	=	em.embq_clifac;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embarque no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
	dw_2.SetItem(il_fila, "embq_nomnav", ls_Nombre)
	dw_2.SetItem(il_fila, "puer_codigo", li_Puerto)
	dw_2.SetItem(il_fila, "reci_nombre", ls_recibidor)
	dw_2.SetItem(il_fila, "embq_clifac", li_recibidor)
	
	istr_mant.Argumento[5]	=	as_Embarque
	istr_mant.Argumento[7]	=	String(li_Destino)
	il_destino					=	li_Destino
	
	RETURN False
END IF
end function

public function boolean buscadestino (string as_embarque, integer ai_tratamiento);Integer	li_Puerto, li_Cliente, li_Destino

li_Cliente	=	Integer(istr_mant.Argumento[3])

SELECT	em.dest_codigo
	INTO	:li_Destino
	FROM	dbo.embarqueprod as em
	WHERE	em.clie_codigo	=	:li_Cliente
	AND	em.embq_codigo	=	:as_Embarque;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embarque no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
	istr_mant.Argumento[5]	=	as_Embarque
	istr_mant.Argumento[7]	=	String(li_Destino)
	il_destino					=	li_Destino
	
	SELECT trat_desref
	INTO :il_desreferencia
	FROM dbo.tratamientos
	WHERE trat_codigo = :ai_tratamiento;
	
END IF
end function

public subroutine buscacliente ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = '1'

OpenWithParm(w_busc_clienprove, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) < 2 THEN Return

IF lstr_busq.argum[1] = "" THEN
//	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clpr_rut[il_fila]		=	lstr_busq.argum[1]
	dw_2.Object.clpr_nombre[il_fila]	=	lstr_busq.argum[2]
	is_rutclie								=	lstr_busq.argum[1]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean existe_plasag (integer ai_planta, long ai_plasag);Long		li_existe, ll_numero, ll_pallet
Integer	li_cliente, li_planta, li_cont

SELECT COUNT(*),max(defe_numero),max(clie_codigo)
	INTO :li_existe,:ll_numero,:li_cliente
	FROM dbo.despafrigoen
	WHERE plde_codigo = :ai_planta
	AND	defe_plasag = :ai_plasag;

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla despafrigoen")
	RETURN True
END IF	

IF li_existe > 0 THEN
	
	li_cont = MessageBox("Existe", "Número de Planilla "+String(ai_plasag)+ " Ya Existe en Despacho Nro. "&
							+String(ll_numero)+" del Cliente "+string(li_cliente)+'.'+"~n~ Desea Continuar",Exclamation!, OKCancel!, 2)

	IF li_cont = 1 THEN
		RETURN False
	ELSE
		RETURN True
	END IF
ELSE	
	RETURN False
END IF	


end function

public function boolean existe_guianula (integer ai_cliente, long al_guia, long al_despacho);Long		li_existe

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.despafrigoguias
	WHERE clie_codigo = :ai_cliente
	AND	defe_guides = :al_guia
	AND	degd_estado = 2;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla despafrigoguias")
	RETURN True
END IF	

IF li_existe > 0 THEN
	MessageBox(	"Atención", "Guía se Encuentra Anulada, Digite Otra.", Information!, Ok!)
	RETURN True
END IF	

li_existe = 0

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.despafrigoguias
	WHERE clie_codigo = :ai_cliente
	AND	defe_guides = :al_guia;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla despafrigoguias")
	RETURN True
END IF

IF li_existe = 0 THEN
	RETURN False
END IF	

li_existe = 0

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.despafrigoguias
	WHERE clie_codigo = :ai_cliente
	AND	defe_guides = :al_guia
	AND	defe_numero = :al_despacho
	AND	dgde_reaper <> 1;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla despafrigoguias")
	RETURN True
END IF

IF li_existe = 0 THEN
	MessageBox(	"Atención", "Guía No Pertenece a Despacho o se Encuentra Abierto, Digite Otra.", Information!, Ok!)
	RETURN True
END IF	




end function

public subroutine cambiaestadopallet (integer ai_cliente, integer ai_planta, long al_pallet, integer ai_cajas);Long		li_existe
Integer	li_cliente, li_planta, ll_pallet, ll_destino, li_control, ll_fila2, ll_Pfruta

UPDATE dbo.palletencab SET
paen_estado = 4,
paen_ccajas = :ai_cajas
WHERE clie_codigo = :ai_cliente
AND	plde_codigo = :ai_planta
AND	paen_numero = :al_pallet
Using (sqlconec);

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab")
	RETURN
END IF	

RETURN



end subroutine

public function boolean pallet_existencia (integer ai_cliente, integer ai_planta, long al_pallet);Long		li_existe
Integer	li_cliente, li_planta, ll_pallet

	SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.palletencab
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	AND	paen_estado = 1
	Using (sqlconec);

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab")
	RETURN True
END IF	

IF li_existe > 0 THEN
	il_palexistencia = 1
	RETURN True
ELSE
	il_palexistencia = 0
	RETURN False
END IF	


end function

public function boolean existe_pallet (integer ai_cliente, integer ai_planta, long al_pallet, long al_guia);Long		li_existe
Integer	li_cliente, li_planta, ll_pallet

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.palletencab
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	AND	paen_estado = 1
	Using (sqlconec);

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab")
	RETURN True
END IF	

IF li_existe > 0 THEN
	il_palexistencia = 1
ELSE
	il_palexistencia = 0
END IF	

SELECT	Count(*)
	INTO	:li_existe
	FROM	dbo.despafrigode as det,dbo.despafrigoen as enc
	WHERE	det.clie_codigo = :ai_cliente
	AND	det.paen_numero = :al_pallet
	AND	det.plde_codigo =	:ai_planta
	AND   enc.defe_tiposa <> 11
	AND	det.clie_Codigo = enc.clie_codigo
	AND 	det.plde_codigo = enc.plde_codigo
	AND	det.defe_numero = enc.defe_numero
	Using (sqlconec);
	
IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla despafrigode")
	RETURN True
END IF	

IF li_existe > 0 THEN
	il_despadestino = 1
ELSE	
	il_despadestino = 0
END IF
	
SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.recfruprocee_trans
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	rfpe_numero = :al_guia
	Using (sqlconec);
	
IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla recfruprocee_trans")
	RETURN True
END IF	

IF li_existe > 0 THEN
	il_exisrecepcion = 1
ELSE	
	il_exisrecepcion = 0
END IF

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.recfruproced_trans
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	AND	rfpe_numero = :al_guia
	Using (sqlconec);
	
IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla recfruproced_trans")
	RETURN True
END IF	

IF li_existe > 0 THEN
	il_exisdettrans = 1
ELSE	
	il_exisdettrans = 0
END IF

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.palletencab_trans
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	Using (sqlconec);
	
IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab")
	RETURN True
END IF	

IF li_existe > 0 THEN
	il_existetrans = 1
ELSE	
	il_existetrans = 0
END IF	

IF li_existe = 0 THEN
	SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.palletencab
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	Using (sqlconec);
END IF	

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab_trans")
	RETURN True
END IF	

IF li_existe > 0 THEN
	il_existepal = 1
ELSE
	il_existepal = 0
END IF	

RETURN False


end function

public function boolean planillaanulada (integer ai_planta, long ai_plasag, string ai_tipoplani);Long		li_existe, ll_numero, ll_pallet
Integer	li_cliente, li_planta, li_cont, li_tipoplani

li_tipoplani = Integer(ai_tipoplani)

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.SAGCORRELANULADOS
	WHERE plde_codigo = :ai_planta
	AND	scoa_numero = :ai_plasag
	AND	scoa_tipoco = :ai_tipoplani;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla SAGCORRELANULADOS")
	RETURN True
END IF	

IF li_existe > 0 THEN
	
	li_cont = MessageBox("Existe", "Número de Planilla "+String(ai_plasag)+ " se Encuentra Anulada Ingrese Otra. ",Exclamation!, OK!)

	RETURN True
ELSE	
	RETURN False
END IF	


end function

public subroutine carga_inspeccion (long pallet, long fila, integer ai_destino);Integer	li_Cliente, li_Planta
Long   	ll_fila, ll_numero, ll_fildet, ll_cuenta, ll_Existe, ll_filanew, ll_filant, ll_filnew, ll_filaant
String 	ls_numero

li_Cliente	= dw_2.Object.clie_codigo[1]
li_Planta	= dw_2.Object.defe_plades[1]

 IF  dw_9.Retrieve(ii_tipoin,il_numins,dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],ii_secuen) > 0 THEN

	ll_numero = dw_2.Object.plde_codigo[1] * 1000000 + il_numins
	
	SELECT Count(*)
		INTO :ll_Existe
		FROM dbo.inspecpalenc_trans
		WHERE inpe_tipoin = :ii_tipoin  AND
				inpe_numero = :ll_numero  AND
				clie_codigo = :li_Cliente AND
				plde_codigo = :li_Planta  AND
				inpe_secuen = :ii_secuen
		USING sqlconec;
				
	IF ll_Existe > 0 THEN 		
	ELSE			
		ll_fila	= dw_11.Find("inpe_tipoin = " + String(ii_tipoin) + &
									 " AND inpe_numero = " + String(ll_numero) + &
							  		 " AND clie_codigo = " + String(dw_2.Object.clie_codigo[1]) + &
							       " AND plde_codigo = " + String(dw_2.Object.defe_plades[1]) + &
							  		 " AND inpe_secuen = " + String(ii_secuen) , 1 , dw_11.RowCount())
							  
		IF ll_fila = 0 THEN
			//dw_9.RowsCopy(1,dw_9.RowCount(), Primary!, dw_11, 1, Primary!)
			
			FOR ll_filaant = 1 TO dw_9.RowCount()
			
				ll_filnew = dw_11.InsertRow(0)
				
				dw_11.Object.clie_codigo[ll_filnew] = li_Cliente
				dw_11.Object.inpe_tipoin[ll_filnew] = ii_tipoin
				dw_11.Object.inpe_numero[ll_filnew] = ll_numero
				dw_11.Object.plde_codigo[ll_filnew] = dw_2.Object.defe_plades[1]
				dw_11.Object.inpe_secuen[ll_filnew] = ii_secuen
				dw_11.Object.dest_codigo[ll_filnew] = ai_destino   
				dw_11.Object.inpe_fechai[ll_filnew] = dw_9.Object.inpe_fechai[ll_filaant]   
				dw_11.Object.espe_codigo[ll_filnew] = dw_9.Object.espe_codigo[ll_filaant]   
				dw_11.Object.vari_codigo[ll_filnew] = dw_9.Object.vari_codigo[ll_filaant]   
				dw_11.Object.emba_codigo[ll_filnew] = dw_9.Object.emba_codigo[ll_filaant]   
				dw_11.Object.tpem_codigo[ll_filnew] = dw_9.Object.tpem_codigo[ll_filaant]   
				dw_11.Object.inpe_todpal[ll_filnew] = dw_9.Object.inpe_todpal[ll_filaant]   
				dw_11.Object.inpe_calibr[ll_filnew] = dw_9.Object.inpe_calibr[ll_filaant]   
				dw_11.Object.inpe_solnom[ll_filnew] = dw_9.Object.inpe_solnom[ll_filaant]
				dw_11.Object.inpe_fecini[ll_filnew] = dw_9.Object.inpe_fecini[ll_filaant]
				dw_11.Object.inpe_fecter[ll_filnew] = dw_9.Object.inpe_fecter[ll_filaant]
				dw_11.Object.tran_fechat[ll_filnew] = dw_9.Object.tran_fechat[ll_filaant]
				dw_11.Object.inpe_desdet[ll_filnew] = dw_9.Object.inpe_desdet[ll_filaant]
				dw_11.Object.inpe_fecres[ll_filnew] = dw_9.Object.inpe_fecres[ll_filaant]
				dw_11.Object.inpe_dessec[ll_filnew] = dw_9.Object.inpe_dessec[ll_filaant]
			
			NEXT
		END IF
	END IF
END IF

ll_cuenta=0

IF dw_10.Retrieve(ii_tipoin,il_numins,dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],ii_secuen,pallet) > 0 THEN

	ll_fildet= dw_12.Find("inpe_tipoin = " + String(ii_tipoin) + &
								 " AND inpe_numero = " + String(ll_numero) + &
								 " AND clie_codigo = " + String(dw_2.Object.clie_codigo[1]) + &
								 " AND plde_codigo = " + String(dw_2.Object.defe_plades[1]) + &
								 " AND inpe_secuen = " + String(ii_secuen) + &
								 " AND paen_numero = " + String(pallet), 1 , dw_12.RowCount())
							  
	IF ll_fildet = 0 THEN
		//dw_10 .RowsCopy(1,dw_10.RowCount(), Primary!, dw_12, 1, Primary!)
		
		FOR ll_filant = 1 TO dw_10.RowCount()
			ll_filanew = dw_12.InsertRow(0) 
			
			dw_12.Object.clie_codigo[ll_filanew] = li_Cliente
			dw_12.Object.inpe_tipoin[ll_filanew] = ii_tipoin
			dw_12.Object.inpe_numero[ll_filanew] = ll_numero
			dw_12.Object.plde_codigo[ll_filanew] = dw_2.Object.defe_plades[1]
			dw_12.Object.inpe_secuen[ll_filanew] = ii_secuen
			dw_12.Object.paen_numero[ll_filanew] = pallet 
			dw_12.Object.dest_codigo[ll_filanew] = ai_destino  
			dw_12.Object.inpd_fechai[ll_filanew] = dw_10.Object.inpd_fechai[ll_filant]  
			dw_12.Object.inpd_nroanu[ll_filanew] = dw_10.Object.inpd_nroanu[ll_filant]  
			dw_12.Object.inpd_fechaa[ll_filanew] = dw_10.Object.inpd_fechaa[ll_filant]  
			dw_12.Object.inpd_frecha[ll_filanew] = dw_10.Object.inpd_frecha[ll_filant] 
			dw_12.Object.tran_fechat[ll_filanew] = dw_10.Object.tran_fechat[ll_filant] 
			dw_12.Object.inpd_nrodev[ll_filanew] = dw_10.Object.inpd_nrodev[ll_filant] 
			dw_12.Object.inpd_fecdev[ll_filanew] = dw_10.Object.inpd_fecdev[ll_filant] 
			dw_12.Object.inpd_nrocer[ll_filanew] = dw_10.Object.inpd_nrocer[ll_filant] 
			dw_12.Object.inpd_fecres[ll_filanew] = dw_10.Object.inpd_fecres[ll_filant] 
		NEXT	
	END IF
END IF
					  		
						 
							 
						 
end subroutine

public subroutine genera_archico_inspeccion ();Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_destinoins, ll_Filas2, ll_nroguia
Integer		li_PldSag, li_cliente, li_planta
String		ls_Archivo, ls_Registro, ls_ruta

dw_inspeccion_deta.reset()
dw_inspeccion_enc.reset()

dw_inspeccion_enc.SetTransObject(Sqlca)
dw_inspeccion_deta.SetTransObject(Sqlca)

dw_inspeccion_enc.Reset()

ll_Numero	= Long(dw_2.Object.defe_numero[1])
li_cliente	= Long(dw_2.Object.clie_codigo[1])
li_planta	= Long(dw_2.Object.plde_codigo[1])

ll_Filas		= dw_inspeccion_enc.Retrieve(Integer(li_cliente), Integer(li_planta), ll_Numero)

ll_Filas2	= dw_inspeccion_deta.Retrieve(Integer(li_cliente), Integer(li_planta), ll_Numero)

IF ll_Filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Recuperación datos de Planilla S.A.G.")

ELSEIF ll_Filas = 0 THEN
	MessageBox("Atención", "No hay información para Planilla Indicada.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	pb_grabar.Enabled	= False

ELSE
	IF dw_inspeccion_enc.retrieve(Integer(li_cliente), Integer(li_planta), ll_Numero) < 1 THEN
		Messagebox("Error", "Despacho NO involucra Inspecciones~n~r" + &
									"No se pudo Generar la Inspecpalenc", Exclamation!)
	ELSE
		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
		ls_Archivo	= '\Inspecpalenc' + String(ll_Numero, '00000000') + '.CSV'
		IF dw_inspeccion_enc.SaveAs(ls_ruta + ls_archivo, CSV!	 ,FALSE) = -1 THEN
			MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especificada~n~r" + &
							ls_ruta + ls_archivo+"~n~r", StopSign!)
			Return

		END IF	
	END IF
	
		IF dw_inspeccion_deta.retrieve(Integer(li_cliente), Integer(li_planta), ll_Numero) < 1 THEN
			Messagebox("Error", "Despacho NO involucra Detalle Inspecciones~n~r" + &
										"No se pudo Generar la Inspecpaldet", Exclamation!)
		ELSE
			RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
			ls_Archivo	= '\Inspecpaldet' + String(ll_Numero, '00000000') + '.CSV'
			IF dw_inspeccion_deta.SaveAs(ls_ruta + ls_archivo, CSV!	 ,FALSE) = -1 THEN
				MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especificada~n~r" + &
								ls_ruta + ls_archivo+"~n~r", StopSign!)
				Return

			END IF	
		END IF
		
	dw_inspeccion_deta.Reset()
	dw_inspeccion_enc.Reset()

END IF

ll_nroguia = dw_2.Object.defe_guides[1]

ls_Archivo	=	"\Guia Despacho-"+String(ll_nroguia)+".xls"
ids_archivo.DataObject	=	'dw_info_anexo_resumen_guia_despacho'
ids_archivo.SetTransObject(sqlca)
ids_archivo.Retrieve(li_cliente,li_planta,ll_nroguia)

//	   vinf.dw_1.DataObject = 'dw_info_anexo_resumen_guia_despacho'
//	   vinf.dw_1.SetTransObject(sqlca)			
//		vinf.dw_1.Retrieve(li_cliente,li_planta,ll_nroguia)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
		
ids_archivo.SaveAs(ls_Ruta + ls_Archivo,excel5!, True)
		
MessageBox("Atención","Archivo Formato Excel, Generado.")


end subroutine

event open;/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio Despacho
						[3]	=	Código de Cliente
						[4]	=	Cantidad de Tarjas
						[5]	=	Código de Embarque
						[7]	=	Código de Destino
						[8]	=	Cantidad de Cajas
						[10]	=	Especie del multipuerto
						[11]	=	Planilla Sag
						[12]	=	multipuerto
						[20]	=	Tipo movimiento
						[20]	=	Guia despacho
*/

/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/
  SELECT empr_coacce
    INTO :ii_controlaaceso  
    FROM dbo.parempresa  
	 USING sqlca;

istr_mant.argumento[8] =" "
x				= 0
y				= 0
This.Height	= 2520
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

iuo_calibre   				=	Create uo_calibre

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("defe_plades", dw_plades)
dw_2.GetChild("puer_codigo", dw_puerto)
dw_2.GetChild("sire_codigo", dw_sitios)
dw_2.GetChild("defe_espmul", idwc_multipuerto)
dw_2.GetChild("defe_especi", idwc_especie)
dw_2.GetChild("defe_tecnic", idwc_cargotecnico)

dw_planta.SetTransObject(sqlca)
dw_plades.SetTransObject(sqlca)
dw_puerto.SetTransObject(sqlca)
dw_sitios.SetTransObject(sqlca)
dw_archplano.SetTransObject(sqlca)
idwc_multipuerto.SetTransObject(sqlca)
idwc_especie.SetTransObject(sqlca)
idwc_cargotecnico.SetTransObject(sqlca)

dw_planta.Retrieve(1)
dw_plades.Retrieve(-1)
dw_puerto.Retrieve(-1)
dw_sitios.Retrieve(-1)
idwc_especie.Retrieve(-1)
//Modificacion
//idwc_multipuerto.Retrieve(-1)
idwc_cargotecnico.Retrieve(gi_codplanta)

dw_2.SetItem(1, "clie_codigo",gi_codexport)
istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)
istr_mant.argumento[7]=''
istr_mant.argumento[27]='7'

pb_nuevo.PostEvent(Clicked!)

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"
GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, 	This.Title, "Acceso a Aplicación", 1)
							
sqlconec						=	CREATE Transaction
sqlconec2					=	CREATE Transaction

IF ii_controlaaceso = 1 THEN
	IF NOT coneccionbase() THEN
		MessageBox("Sin Conexión", "No Existe Conexión a Base Control de Acceso.", StopSign!, Ok!)	
	ELSE
		dw_2.Object.defe_patent.Dddw.Name			=	'dw_mues_controlacceso'
		dw_2.Object.defe_patent.Dddw.DisplayColumn	=	'ctac_patent'
		dw_2.Object.defe_patent.Dddw.DataColumn	=	'ctac_patent'
		dw_2.Object.defe_patent.Dddw.AllowEdit		=  True
		dw_2.Object.defe_patent.Dddw.HScrollBar		=  True
		dw_2.Object.defe_patent.Dddw.VScrollBar		=  True
		dw_2.Object.defe_patent.Dddw.Case 			= 	"Upper"
		dw_2.Modify("defe_patent.dddw.Limit=10")
		dw_2.Modify("defe_patent.Dddw.PercentWidth=250")
	
		dw_2.GetChild("defe_patent", idwc_patente)
		idwc_patente.SetTransObject(sqlconec2)
		idwc_patente.Retrieve()
	END IF
END IF

dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)
dw_10.SetTransObject(sqlca)

dw_palletfrutahisto.SetTransObject(sqlca)
dw_palletencahisto.SetTransObject(sqlca)
dw_alpalletfruta.SetTransObject(sqlca)
dw_alpalletencab.SetTransObject(sqlca)

iuo_patente					=	Create uo_patente
iuo_Especie					=	Create uo_Especie
iuo_clprv						=	Create uo_clienprove
iuo_Cliente					=	Create uo_ClientesProd	
iuo_Guia						=	Create uo_GuiaDespacho
iuo_Transportista			=	Create uo_transportista
iuo_TransportistaServ	=	Create uo_transportista


// Para guardar fecha de despacho en la Palletfruta
ids_palletfruta_fecha				=	Create	DataStore
ids_palletfruta_fecha.DataObject	=	'dw_mues_palletfruta_pafrfecdes'
ids_palletfruta_fecha.SetTransObject(sqlca)

ids_CorrelMovim					=	Create	DataStore
ids_CorrelMovim.DataObject	=	'dw_mues_correlmoviemientos_despa'
ids_CorrelMovim.SetTransObject(sqlca)

dw_14.SetTransObject(sqlca)
end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

istr_mant.argumento[10] = String(dw_2.Object.defe_especi[1])
istr_mant.argumento[11] = String(dw_2.Object.defe_plasag[1])
istr_mant.argumento[12] = String(dw_2.Object.defe_espmul[1])

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

Integer	li_cajas

istr_mant.argumento[10] = String(dw_2.Object.defe_especi[1])
istr_mant.argumento[11] = String(dw_2.Object.defe_plasag[1])
istr_mant.argumento[12] = String(dw_2.Object.defe_espmul[1])
istr_mant.argumento[20] = String(dw_2.Object.defe_tiposa[1])
istr_mant.argumento[25] = String(dw_2.Object.defe_guides[1])

IF dw_1.RowCount() >= dw_2.Object.defe_cantar[1] THEN
	MessageBox("Atención", "No puede ingresar más Pallets.")
ELSE
	IF dw_1.RowCount() > 0 THEN
		li_cajas	=	dw_1.Object.totcajas[1]
		IF dw_1.Object.totcajas[1] >= Integer(istr_mant.Argumento[8]) THEN
			MessageBox("Atención", "Se ha completado la cantidad de Cajas, No podrá ingresar más Pallets.")
			RETURN
		END IF
	END IF
	
	IF istr_mant.argumento[20] = '31' THEN
		OpenWithParm(w_mant_deta_despafrigode_usda, istr_mant)		
	ELSE	
		OpenWithParm(iw_mantencion, istr_mant)
	END IF
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_folio, ll_null, ll_guia
integer	li_respuesta, li_cliente, li_planta, li_cont

SetNUll(ll_null)

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[3]), Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]))
	
	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]))

			If ll_fila_d = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Else
				pb_imprimir.Enabled	= True
				
				dw_2.SetTabOrder("clie_codigo",0)
				dw_2.SetTabOrder("plde_codigo",0)
				
				If dw_2.Object.defe_estado[1]	=	1	Then
					istr_mant.solo_consulta	=	True
				Else
					If ll_fila_d >  0 Then
						li_cliente = Integer(istr_mant.argumento[3])
						li_planta  = Integer(istr_mant.argumento[1])
						ll_folio	  = Long(istr_mant.argumento[2])
						ll_guia	  = dw_2.Object.defe_guides[1]
						
						If dw_2.Object.defe_tiposa[1] = 7 Or dw_2.Object.defe_tiposa[1] = 8 Or dw_2.Object.defe_tiposa[1] = 9 Then 
							NoExisteEmbarque(dw_2.Object.embq_codigo[1])
						End If
						
						iuo_Cliente.Existe(dw_2.Object.clie_codigo[1], False, Sqlca)
		
						If gi_Emisor_Electronico = 1 And iuo_Cliente.Guia_Electronica = 1 And &
								(dw_2.Object.defe_tiposa[1] = 7 Or dw_2.Object.defe_tiposa[1] = 8 Or dw_2.Object.defe_tiposa[1] = 9 Or &
								dw_2.Object.defe_tiposa[1] = 10 Or dw_2.Object.defe_tiposa[1] = 11 Or dw_2.Object.defe_tiposa[1] = 30)Then 
								
							dw_2.Object.defe_guides.Protect	= 1
							dw_2.Object.defe_guides.Color		= Rgb(255,255,255)
							dw_2.Object.defe_guides.BackGround.Color	= 553648127
						Else
							dw_2.Object.defe_guides.Protect 	= 0
							dw_2.Object.defe_guides.Color		= 0
							dw_2.Object.defe_guides.BackGround.Color	= Rgb(255,255,255)
						End If
						
						SELECT count(*) 
						INTO :li_cont
						FROM dbo.despafrigoguias
						WHERE clie_codigo = :li_cliente
							AND 	plde_codigo = :li_planta
							AND 	defe_numero = :ll_folio
							AND	defe_guides = :ll_guia 
							AND 	dgde_reaper = 1; 
						Commit;
									
						If li_cont > 0 Then
							If dw_2.Object.defe_tiposa[1] <> 15 Then
								li_respuesta = MessageBox("Atención", "Despacho se Encuentra Abierto, Debe Cambiar Guía, Desea Continuar", Exclamation!, YESNO!, 2)
								
								If li_respuesta = 1 Then							
									dw_2.SetItem(1, 'defe_guides', ll_Null)
									dw_2.Object.defe_guiaem[1] = 0
									If Not iuo_Guia.of_GeneraAnulaGuia(li_cliente, li_planta, ll_guia, ll_folio, 70, &
																		dw_2.Object.embq_codigo[1], 1, 'Por Apertura de Despacho.') Then
										MessageBox('Error', 'No se pudo cargar la tabla de Guias Nulas')
									End If								
									il_cierra = 2
								Else							
									il_cierra = 1
									Return
								End If
							End If
						End If	
					End If
					
					istr_mant.solo_consulta 	=	False
					pb_eli_det.Enabled		=	True
					pb_ins_det.Enabled		=	True
					pb_grabar.Enabled		=	True
					pb_eliminar.Enabled		=	True
				End If
								
				If ll_fila_d > 0 Then
					pb_imprimir.Enabled	= True
					
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					habilitaIngreso("defe_guides")
				Else
					If dw_2.Object.defe_estado[1]	=	1 Then pb_ins_det.Enabled	=	True							
				End If
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)

end event

on w_maed_despafrigoen.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_7=create dw_7
this.dw_8=create dw_8
this.dw_9=create dw_9
this.dw_10=create dw_10
this.dw_11=create dw_11
this.dw_12=create dw_12
this.dw_13=create dw_13
this.dw_14=create dw_14
this.dw_15=create dw_15
this.dw_alpalletencab=create dw_alpalletencab
this.dw_alpalletfruta=create dw_alpalletfruta
this.dw_palletencahisto=create dw_palletencahisto
this.dw_palletfrutahisto=create dw_palletfrutahisto
this.dw_archplano=create dw_archplano
this.dw_16=create dw_16
this.dw_6=create dw_6
this.dw_inspeccion_enc=create dw_inspeccion_enc
this.dw_inspeccion_deta=create dw_inspeccion_deta
this.cb_limpia=create cb_limpia
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_5
this.Control[iCurrent+4]=this.dw_7
this.Control[iCurrent+5]=this.dw_8
this.Control[iCurrent+6]=this.dw_9
this.Control[iCurrent+7]=this.dw_10
this.Control[iCurrent+8]=this.dw_11
this.Control[iCurrent+9]=this.dw_12
this.Control[iCurrent+10]=this.dw_13
this.Control[iCurrent+11]=this.dw_14
this.Control[iCurrent+12]=this.dw_15
this.Control[iCurrent+13]=this.dw_alpalletencab
this.Control[iCurrent+14]=this.dw_alpalletfruta
this.Control[iCurrent+15]=this.dw_palletencahisto
this.Control[iCurrent+16]=this.dw_palletfrutahisto
this.Control[iCurrent+17]=this.dw_archplano
this.Control[iCurrent+18]=this.dw_16
this.Control[iCurrent+19]=this.dw_6
this.Control[iCurrent+20]=this.dw_inspeccion_enc
this.Control[iCurrent+21]=this.dw_inspeccion_deta
this.Control[iCurrent+22]=this.cb_limpia
end on

on w_maed_despafrigoen.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.dw_9)
destroy(this.dw_10)
destroy(this.dw_11)
destroy(this.dw_12)
destroy(this.dw_13)
destroy(this.dw_14)
destroy(this.dw_15)
destroy(this.dw_alpalletencab)
destroy(this.dw_alpalletfruta)
destroy(this.dw_palletencahisto)
destroy(this.dw_palletfrutahisto)
destroy(this.dw_archplano)
destroy(this.dw_16)
destroy(this.dw_6)
destroy(this.dw_inspeccion_enc)
destroy(this.dw_inspeccion_deta)
destroy(this.cb_limpia)
end on

event ue_nuevo;HabilitaEncab(True)


ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
dw_9.Reset()
dw_10.Reset()
dw_11.Reset()
dw_12.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled		= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetTabOrder("clie_codigo",1)
dw_2.SetTabOrder("plde_codigo",2)
dw_2.SetTabOrder("defe_espmul",0)
dw_2.Object.defe_espmul.Color	= 0
dw_2.Object.defe_espmul.BackGround.Color	= 553648127

dw_planta.Retrieve(1)
dw_plades.Retrieve(1)
dw_puerto.Retrieve(1)
dw_sitios.Retrieve(1)
idwc_especie.Retrieve()
idwc_multipuerto.Retrieve(-1)
idwc_cargotecnico.Retrieve(gi_codplanta)
				
dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)
dw_2.SetItem(1,"defe_horade", Now())
dw_2.SetItem(1,"defe_oricam",1)

iuo_Cliente.Existe(gi_CodExport, False, Sqlca) 

If gi_Emisor_Electronico = 1 And iuo_Cliente.Guia_Electronica = 1 And &
	(dw_2.Object.defe_tiposa[1] = 7 Or dw_2.Object.defe_tiposa[1] = 8 Or dw_2.Object.defe_tiposa[1] = 9 Or &
	dw_2.Object.defe_tiposa[1] = 10 Or dw_2.Object.defe_tiposa[1] = 11 Or dw_2.Object.defe_tiposa[1] = 30) Then
	dw_2.Object.defe_guides.Protect	= 1
	dw_2.Object.defe_guides.Color		= Rgb(255,255,255)
	dw_2.Object.defe_guides.BackGround.Color	= 553648127
Else
	dw_2.Object.defe_guides.Protect	= 0
	dw_2.Object.defe_guides.Color		= 0
	dw_2.Object.defe_guides.BackGround.Color	= Rgb(255,255,255)
End If

dw_2.Object.defe_guiaem[1] = 0

istr_mant.argumento[3]= String(gi_CodExport)

dw_2.SetRedraw(True)
dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;Str_busqueda			lstr_busq

lstr_busq.argum[1]	= 	istr_mant.argumento[3]
lstr_busq.argum[2]	= 	istr_mant.argumento[1]

OpenWithParm(w_busc_despafrigoen, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= lstr_busq.argum[2]
	istr_mant.argumento[2]	= lstr_busq.argum[5]
	istr_mant.argumento[3]  = lstr_busq.argum[1]
	ib_existe_folioD			=	True
	This.TriggerEvent("ue_recuperadatos")
	
	IF il_cierra = 1 THEN
		Return
	END IF	
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= False
	istr_mant.borra			= False
	istr_mant.Argumento[6]	=	String(dw_1.Object.paen_numero[il_fila])
	istr_mant.argumento[10] = String(dw_2.Object.defe_especi[1])
	istr_mant.argumento[11] = String(dw_2.Object.defe_plasag[1])
	istr_mant.argumento[12] = String(dw_2.Object.defe_espmul[1])
	
	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_filas, ll_fila_d,ll_fila_g, ll_nuevofolio, li_fillas, ll_destino, ll_guiades

ll_destino 	= 	dw_2.Object.defe_plades[1]
ll_guiades	=	dw_2.Object.defe_guides[1]


If dw_2.Object.defe_tiposa[1] <> 33 Then // Salida Tipo Siniestro
	If IsNull(dw_2.Object.defe_chfrut[1]) OR dw_2.Object.defe_chfrut[1] = '' Then
		MessageBox("Error de Consistencia", "Falta Número RUT Chofer.", StopSign!, Ok!)
		dw_2.SetColumn("defe_chfrut")
		Message.DoubleParm = -1
	End If	
	
	If IsNull(dw_2.Object.defe_celcho[1]) OR dw_2.Object.defe_celcho[1] = '' Then
		MessageBox("Error de Consistencia", "Falta Número Celular Chofer.", StopSign!, Ok!)
		dw_2.SetColumn("defe_celcho")
		Message.DoubleParm = -1
	End If	
	
	If IsNull(dw_2.Object.defe_chofer[1]) OR dw_2.Object.defe_chofer[1] = '' Then
		MessageBox("Error de Consistencia", "Falta Nombre Chofer.", StopSign!, Ok!)
		dw_2.SetColumn("defe_chofer")
		Message.DoubleParm = -1
	End If	
End If
	
If dw_2.Object.tica_codigo[1] = 2 Then
	If IsNull(dw_2.Object.defe_selnav[1]) OR dw_2.Object.defe_selnav[1] = '' Then
		MessageBox("Error de Consistencia", "Falta Sello Cia. Naviera.", StopSign!, Ok!)
		dw_2.SetColumn("defe_selnav")
		Message.DoubleParm = -1
	End If	
	
	If IsNull(dw_2.Object.defe_Nrcont[1]) OR dw_2.Object.defe_Nrcont[1] = '' Then
		MessageBox("Error de Consistencia", "Falta Número Contenedor.", StopSign!, Ok!)
		dw_2.SetColumn("defe_Nrcont")
		Message.DoubleParm = -1
	End If	
	If IsNull(dw_2.Object.defe_setpoi[1]) OR dw_2.Object.defe_setpoi[1] = 0 Then
		MessageBox("Error de Consistencia", "Falta Seteo T°.", StopSign!, Ok!)
		dw_2.SetColumn("defe_setpoi")
		Message.DoubleParm = -1
	End If	
	If IsNull(dw_2.Object.defe_ventil[1]) OR dw_2.Object.defe_ventil[1] = 0 Then
		MessageBox("Error de Consistencia", "Falta A. Lampa.", StopSign!, Ok!)
		dw_2.SetColumn("defe_ventil")
		Message.DoubleParm = -1
	End If
	If IsNull(dw_2.Object.defe_fecfab[1]) OR dw_2.Object.defe_fecfab[1] = Date('19000101') Then
		MessageBox("Error de Consistencia", "Falta Fecha Fabricación.", StopSign!, Ok!)
		dw_2.SetColumn("defe_fecfab")
		Message.DoubleParm = -1
	End If
	If IsNull(dw_2.Object.defe_orcont[1]) OR dw_2.Object.defe_orcont[1] = 0 Then
		MessageBox("Error de Consistencia", "Falta Orden.", StopSign!, Ok!)
		dw_2.SetColumn("defe_orcont")
		Message.DoubleParm = -1
	End If
	If IsNull(dw_2.Object.defe_tipoca[1]) OR dw_2.Object.defe_tipoca[1] = '' Then
		MessageBox("Error de Consistencia", "Falta Tipo Carga.", StopSign!, Ok!)
		dw_2.SetColumn("defe_tipoca")
		Message.DoubleParm = -1
	End If
	If IsNull(dw_2.Object.defe_tipoat[1]) OR dw_2.Object.defe_tipoat[1] = 0 Then
		MessageBox("Error de Consistencia", "Falta Tipo Atmosfera.", StopSign!, Ok!)
		dw_2.SetColumn("defe_tipoat")
		Message.DoubleParm = -1
	End If
	
	If IsNull(dw_2.Object.tpco_codigo[1]) OR dw_2.Object.tpco_codigo[1] = 0 Then
		MessageBox("Error de Consistencia", "Falta Tipo Contenedor.", StopSign!, Ok!)
		dw_2.SetColumn("tpco_codigo")
		Message.DoubleParm = -1
	End If
	
	If IsNull(dw_2.Object.defe_taraco[1]) OR dw_2.Object.defe_taraco[1] = 0 Then
		MessageBox("Error de Consistencia", "Falta Tara del Camion.", StopSign!, Ok!)
		dw_2.SetColumn("tpco_codigo")
		Message.DoubleParm = -1
	End If
End If	

If dw_2.Object.defe_consol[1] = 1 Then
	If IsNull(dw_2.Object.defe_plades[1]) OR dw_2.Object.defe_plades[1] = 0 Then
		MessageBox("Error de Consistencia", "Falta Planta Destino.", StopSign!, Ok!)
		dw_2.SetColumn("defe_plades")
		Message.DoubleParm = -1
	End If	
End If	

If IsNull(dw_2.Object.defe_oricam[1]) OR dw_2.Object.defe_oricam[1] = 0 Then
	MessageBox("Error de Consistencia", "Falta Origen patente camión.", StopSign!, Ok!)
	dw_2.SetColumn("defe_oricam")
	Message.DoubleParm = -1
End If	

If dw_2.GetNextModIfied(0, Primary!) > 0 Then
	dw_2.SetItem(1, "defe_fecact", Today())
	dw_2.SetItem(1, "defe_horact", Now())
End If

If dw_2.Object.defe_tiposa[1] = 30 Then
	If IsNull(dw_2.Object.sire_codigo[1]) OR dw_2.Object.sire_codigo[1] = 0 Then
		MessageBox("Error de Consistencia", "Falta Codigo Sitio Revision USDA.", StopSign!, Ok!)
		dw_2.SetColumn("sire_codigo")
		Message.DoubleParm = -1
	End If	
End If

If gi_Emisor_Electronico = 1 And iuo_Cliente.Guia_Electronica = 1 And &
	(dw_2.Object.defe_tiposa[1] = 7 Or dw_2.Object.defe_tiposa[1] = 8 Or dw_2.Object.defe_tiposa[1] = 9 Or &
	dw_2.Object.defe_tiposa[1] = 10 Or dw_2.Object.defe_tiposa[1] = 11 Or dw_2.Object.defe_tiposa[1] = 30) Then
Else
	If  dw_2.Object.defe_tiposa[1]  <> 33 Then
		If IsNull(dw_2.Object.defe_guides[1]) Or dw_2.Object.defe_guides[1] = 0 Then
			MessageBox("Error de Consistencia", "Falta Número Guía Despacho.", StopSign!, Ok!)
			dw_2.SetColumn("defe_guides")
			Message.DoubleParm = -1
		End If	
	End If
End If

If Message.DoubleParm = -1 Then Return

ids_palletfruta_fecha.Reset() 
dw_6.Reset()

If dw_2.Object.defe_tiposa[1] <> 11  Then
	For ll_filas	=	1	To	dw_1.RowCount()
		If	dw_6.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_filas],dw_2.Object.plde_codigo[1],1) > 0 Then
			For ll_fila_d = 1 To dw_6.RowCount()
				ll_fila_g	=	ids_palletfruta_fecha.InsertRow(0)
				ids_palletfruta_fecha.SetItem(ll_fila_g,'clie_codigo',dw_6.Object.clie_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'paen_numero',dw_6.Object.paen_numero[ll_fila_d])						
				ids_palletfruta_fecha.SetItem(ll_fila_g,'espe_codigo',dw_6.Object.espe_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'vari_codigo',dw_6.Object.vari_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'emba_codigo',dw_6.Object.emba_codigo[ll_fila_d])						
				ids_palletfruta_fecha.SetItem(ll_fila_g,'prod_codigo',dw_6.Object.prod_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'cond_codigo',dw_6.Object.cond_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'etiq_codigo',dw_6.Object.etiq_codigo[ll_fila_d])						
				ids_palletfruta_fecha.SetItem(ll_fila_g,'plde_codigo',dw_6.Object.plde_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'pafr_calibr',dw_6.Object.pafr_calibr[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'pafr_secuen',dw_6.Object.pafr_secuen[ll_fila_d])		
				ids_palletfruta_fecha.SetItem(ll_fila_g,'pafr_fecdes',dw_2.Object.defe_fecdes[1])				
			Next
		End If
	Next
	//dw_6.Reset()
	
	//DwItemStatus	Estadol 
	ids_palletfruta_fecha.ResetUpdate()
	
	For ll_filas 	=	1 To 	ids_palletfruta_fecha.RowCount()
		//Estadol	=	ids_palletfruta_fecha.GetItemStatus(ll_filas, 0, Primary!)
		//ids_palletfruta_fecha.SetItemStatus(ll_filas,0,  Primary!, DataModIfied!	)	
		ids_palletfruta_fecha.SetItemStatus(ll_filas,'pafr_fecdes',  Primary!, DataModIfied!	)	
	Next
End If

dw_6.Reset()

If Not ib_existe_folioD	Then
	If IsNull(dw_2.Object.defe_numero[1]) OR dw_2.Object.defe_numero[1] = 0 Then
		ll_nuevofolio=BusNuevoFolioDespa(Integer(istr_mant.Argumento[3]),Integer(istr_mant.Argumento[1]))
		il_Folio	=	ll_nuevofolio
		
		If il_Folio > 0 Then
			dw_2.Object.defe_numero[1]	= ll_nuevofolio
			dw_2.SetItem(1, "defe_numero",ll_nuevofolio)
		
			istr_mant.Argumento[2]	= String(ll_nuevofolio)
			
			For li_fillas = 1 To dw_1.RowCount()
				 dw_1.Object.defe_numero[li_fillas]	= ll_nuevofolio
			Next		
		End If
	End If	
Else
	il_Folio	=	dw_2.Object.defe_numero[1]
End If
end event

event ue_guardar;Integer 			li_codigo, li_planta, li_cliente, li_control, ll_ClienteNew
Long				ll_numero, ll_Guia 
String				ls_usuario, ls_computador
Str_Busqueda	lstr_Busq

If dw_1.AcceptText() = -1 Then RETURN

SetPointer(HourGlass!)
w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")
If Message.DoubleParm = -1  Then RETURN

If gi_Emisor_Electronico = 1 And iuo_Cliente.Guia_Electronica = 1 And &
	(dw_2.Object.defe_tiposa[1] = 7 Or dw_2.Object.defe_tiposa[1] = 8 Or dw_2.Object.defe_tiposa[1] = 9 Or &
	dw_2.Object.defe_tiposa[1] = 10 Or dw_2.Object.defe_tiposa[1] = 11 Or dw_2.Object.defe_tiposa[1] = 30)Then
	If dw_2.Object.defe_guiaem[1] = 0 Or dw_2.Object.defe_guiaem[1] = 2 Then 
		ll_Guia = iuo_Guia.of_Emiteguia(dw_2.Object.plde_codigo[1], dw_2.Object.clie_codigo[1], 1)
		If ll_Guia > 0 Then
			If Not Existe_GuiaNula(dw_2.Object.clie_codigo[1], ll_Guia, dw_2.Object.defe_numero[1]) Then
				dw_2.Object.defe_guides[1] = ll_Guia
				dw_2.Object.defe_guiaem[1] = 3
			Else 
				Message.DoubleParm = -1	
			End If		
		Else
			MessageBox('Alerta', 'No se pudo obtener numero de guias de despacho.', Exclamation!, OK!)
			Message.DoubleParm = -1
		End If
	End If
End If

If wf_actualiza_db(False) Then
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	//Envia correo de despacho interplanta.
	If dw_2.Object.defe_tiposa[1] = 31 OR dw_2.Object.defe_tiposa[1] = 11 Then EnviaMail()
	
	TriggerEvent("ue_despuesgrabar")
	
	If dw_2.Object.defe_traspa[1]  = 1 Then
		li_control = MessageBox("Atención","Esta Seleccionando Traspasar Fruta a Otro Cliente,~n" + & 
											" debe asegurarse de tener creados los embalajes nuevo Cliente.~nDesea Continuar",Exclamation!, YesNO!, 2)
		If li_Control = 1 Then
				ls_usuario 		= gstr_us.Nombre			
				ls_computador 	= gstr_us.Computador	
				li_cliente			= dw_2.Object.clie_codigo[1]
				li_planta			= dw_2.Object.plde_codigo[1]
				ll_numero		= dw_2.Object.defe_numero[1]
			
				Open(w_cliente)
			
				lstr_Busq = Message.PowerObjectParm
				
				ll_ClienteNew	= Integer(lstr_Busq.Argum[1])	
				
				DECLARE Traspaso_Cliente PROCEDURE FOR dbo.FProc_TraspasoClientes	
						  @cliente 		= :li_cliente,
						  @planta 		= :li_planta,
						  @numero 		= :ll_numero,
						  @usuario		= :ls_usuario, 
						  @computador	= :ls_computador,
						  @ClienteNew = :ll_ClienteNew
				USING SQLCA;
						  
				EXECUTE Traspaso_Cliente;
							
				If Sqlca.SQLCode = -1 Then
					F_ErrorBaseDatos(sqlca, "Lectura SP Traspaso Cliente. Debera Crear Recepcion de Forma Manual.")
					Return
				End If	
			
			Close Traspaso_Cliente;
			
			MessageBox('Atencion...', 'Se creo recepcion para Cliente:' + String(ll_ClienteNew, '000'), Information!, Ok!)
		End If
	End If
	
	If dw_2.Object.defe_frucom[1]  = 1 Then
		
		li_control = MessageBox("Atención","Esta Seleccionando Traspasar Fruta a Comercial. Desea Continuar",Exclamation!, YesNO!, 2)
		
		If li_control = 1 Then
		
			ls_usuario 		= gstr_us.Nombre			
			ls_computador 	= gstr_us.Computador	
			li_cliente			= dw_2.Object.clie_codigo[1]
			li_planta			= dw_2.Object.plde_codigo[1]
			ll_numero		= dw_2.Object.defe_numero[1]		
			
			DECLARE Traspaso_comercial PROCEDURE FOR dbo.Fproc_traspasofrutacomercial	
					  @Cliente 		= :li_cliente,
					  @planta 		= :li_planta,
					  @numero 		= :ll_numero,
					  @usuario		= :ls_usuario, 
					  @computador	= :ls_computador;
					  
			EXECUTE Traspaso_comercial;
						
			If SQLCA.SQLCode = -1 Then
				F_ErrorBaseDatos(sqlca, "Lectura SP Traspaso Fruta Comercial.")
				Return
			End If	
			 
			Commit;
			
			Close Traspaso_comercial;
			
			MessageBox("Atención","Traspaso a Fruta a Comercial Terminado.",Exclamation!)
		End If
	End If
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
End If 
	
If dw_2.Object.defe_tiposa[1] = 11 Then
	
	SELECT empr_codtra  
   		INTO :li_codigo  
    		FROM dbo.parempresa
		Using SQLCA;

	If li_codigo = 1 Then
   		Message.DoubleParm = 0
		If ConexionBase() Then
		   //Existe_CargaRegistro()
			TriggerEvent("ue_despuesguardar")
			If Message.DoubleParm = -1 OR il_Folio	=	0 Then RETURN
			
			If wf_actualiza_Carga(False) AND il_frena = 0 Then
			   Messagebox("Atención","Traspaso de Despacho a Planta Seleccionada Realizada En Forma Satisfactoria",exclamation!) 
				w_main.SetMicroHelp("Información Grabada.")
				il_frena = 0
				DISCONNECT USING sqlconec;
			Else
				Messagebox("Atención","No se pudo transmitir Despacho a Planta Seleccionada, Se Generará Archivo Plano",exclamation!) 
				w_main.SetMicroHelp("No se puede Grabar información.")
				DISCONNECT USING sqlconec;
				il_frena = 0
				//GeneraArchivoPlano()
			End If
		Else
			MessageBox("Sin Conexión", "No Existe Conexión a Zona, Se Procederá a Generar Archivo Plano", StopSign!, Ok!)	
			il_frena = 0
   		//GeneraArchivoPlano()
		End If
	 Else
		Messagebox("Atención","No se pudo transmitir Despacho a Planta Seleccionada, Se Generará Archivo Plano",exclamation!) 
		il_frena = 0
	   //GeneraArchivoPlano() 	
	 End If
End If
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
	IF dw_2.Object.defe_estado[1]	<>	1	THEN	pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		IF il_cierra <> 2 THEN
			pb_Grabar.Enabled		=	True
		END IF	
		pb_ins_det.Enabled	=	True
		pb_eli_det.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSE
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_despafrigoen
integer x = 18
integer y = 1220
integer width = 3095
integer height = 892
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_despafrigode"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_despafrigoen
integer x = 46
integer y = 16
integer width = 2976
integer height = 1148
string dataobject = "dw_mant_despafrigoen"
boolean livescroll = true
boolean righttoleft = true
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null, ll_Planilla, ll_tpcont, ll_orcont, ll_guides, ll_sello, ll_despacho,ll_totkgb, ll_taraco, ll_totvgm
String	ls_columna, ls_null, ls_embq_codigo, ls_TipoPla, ls_digito, ls_digito1,&
			ls_glosa,ls_glosag,ls_ubisel,ls_numsel,ls_nrcont,ls_patent,ls_pataco, ls_chofer, ls_rutchofer, ls_sagmultipuerto
Date		ld_nula
Integer	li_Cliente, li_Planta, li_existe, li_espmul, li_sicodigo, li_trancodigo, li_ticacodigo, li_tipocontenedor

SetNull(ll_null)
SetNull(ls_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE Case ls_columna
	Case "clie_codigo"
		istr_mant.argumento[3]	= data
		If F_ValidaCliente(Integer(data)) = False Then
			dw_2.SetItem(il_fila, "clie_codigo", gi_codexport)
			Return 1
		Else
			dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
			
			dw_2.SetItem(1, "embq_nomnav", ls_null)
			dw_2.SetItem(1, "embq_codigo", ls_null)
			
			dw_2.GetChild("defe_plades", dw_plades)
			dw_plades.SetTransObject(sqlca)
			dw_plades.Retrieve(1)
			
			iuo_Cliente.Existe(Long(Data), False, Sqlca)
			If iuo_Cliente.Guia_Electronica = 1 And iuo_Cliente.Guia_Electronica = 1 And &
			(This.Object.defe_tiposa[1] = 7 Or This.Object.defe_tiposa[1] = 8 Or This.Object.defe_tiposa[1] = 9 Or &
			This.Object.defe_tiposa[1] = 10 Or This.Object.defe_tiposa[1] = 11 Or This.Object.defe_tiposa[1] = 30)Then 
				This.Object.defe_guides.Protect	= 1
				This.Object.defe_guides.Color		= Rgb(255,255,255)
				This.Object.defe_guides.BackGround.Color	= 553648127
				This.SetItem(Row, "defe_guides", Long(ls_Null))
			Else
				This.Object.defe_guides.Protect 	= 0
				This.Object.defe_guides.Color		= 0
				This.Object.defe_guides.BackGround.Color	= Rgb(255,255,255)
			End If
		End If
					
	Case "plde_codigo"
		ExisteFolio(ls_columna, data)
		dw_2.GetChild("defe_plades", dw_plades)
		dw_plades.SetTransObject(sqlca)
		dw_plades.Retrieve(1)
		
		dw_2.GetChild("defe_tecnic", idwc_cargotecnico)
		idwc_cargotecnico.SetTransObject(sqlca)
		idwc_cargotecnico.Retrieve(Integer(data))	
		
		istr_mant.argumento[1] = Data
		
	Case "defe_numero"
		//ExisteFolio(ls_columna, data)
		If ExisteFolio(ls_columna, data) Then
			This.SetItem(1, ls_columna, Integer(ls_null))
			Return 1
		End If
		
		If il_cierra = 1 Then
			Pb_salir.TriggerEvent(Clicked!)
			Return
		End If
		
	Case "defe_cantar"
		istr_mant.argumento[4]	= data
		
	Case "defe_guides"
		If IsNull(dw_2.Object.defe_numero[1]) Then
			ll_despacho = 0
		Else
			ll_despacho = dw_2.Object.defe_numero[1]
		End If
		
		If Existe_GuiaNula(dw_2.Object.clie_codigo[1], Long(data),ll_despacho) Then
			This.SetItem(1, ls_columna, Long(ls_null))
			Return 1
		End If
			
	Case "defe_cancaj"
		istr_mant.argumento[8]	= data
		
	Case "embq_codigo"
		If NoExisteEmbarque(data) Then
			This.Object.embq_nomnav[row]	=	""
			This.Object.embq_codigo[row]	=	ls_null
			HabilitaIngreso(ls_columna)
			Return 1
		End If

	Case "defe_tiposa"
		istr_mant.argumento[27] = data
		
		If data = '10' Then This.Object.defe_traspa[1]	=	0
		If data = '33' Then This.Object.tica_codigo[1]	=	4
		
		If data <> '11' Then
			This.Object.defe_plades[1]	=	Integer(ll_null)
		Else
			This.Object.puer_codigo[1]		=	Integer(ll_null)
			This.Object.embq_codigo[1]	=	String(ll_null)
			This.Object.embq_nomnav[1]	=	String(ll_null)
		End If	
		
		If data <> '7' Then
			dw_2.Object.defe_plades[1] = Integer(ll_null)
			dw_2.Object.defe_consol[1] = Integer(ll_null)
		End If
		
		iuo_Cliente.Existe(This.Object.clie_codigo[1], False, Sqlca)
		
		If gi_Emisor_Electronico = 1 And iuo_Cliente.Guia_Electronica = 1 And &
				(Integer(Data) = 7 Or Integer(Data) = 8 Or Integer(Data) = 9 Or &
				Integer(Data) = 10 Or Integer(Data) = 11 Or Integer(Data) = 30)Then 
				
			This.Object.defe_guides.Protect	= 1
			This.Object.defe_guides.Color		= Rgb(255,255,255)
			This.Object.defe_guides.BackGround.Color	= 553648127
		Else
			This.Object.defe_guides.Protect 	= 0
			This.Object.defe_guides.Color		= 0
			This.Object.defe_guides.BackGround.Color	= Rgb(255,255,255)
		End If
		
	Case "defe_fecdes"
		If Not f_validafechatempo(date(data)) Then
			This.SetItem(Row, ls_Columna, ld_nula)
			Return 1
		End If	
		
Case "defe_nturno"		
	This.SetItem(1, "defe_plasag", Integer(ls_null))
		
Case "defe_plasag"
	If existe_plasag(Integer(istr_mant.argumento[1]),Long(data)) Then
		This.SetItem(1, ls_columna, Integer(ls_null))
		Return 1
	End If
	
	If IsNull(dw_2.Object.defe_nturno[1]) Then
		MessageBox("Atención","Falta Ingreso de Tipo Planilla.")
		This.SetItem(1, ls_columna, Integer(ls_null))
		Return 1
	End If	
	
	If PlanillaAnulada(Integer(istr_mant.argumento[1]),Long(data),dw_2.Object.defe_nturno[1]) Then
		This.SetItem(1, ls_columna, Integer(ls_null))
		Return 1
	End If
	
	If Not IsNull(data) Then
		dw_2.Object.defe_espmul.Protect	=	0
		dw_2.Object.defe_espmul.Color 	=	0
		dw_2.Object.defe_espmul.BackGround.Color = Rgb(255,255,255)
		
		ll_Planilla						=	Long(data)
		ls_TipoPla						=	dw_2.Object.defe_nturno[1]
		dw_2.Object.defe_nrosps[1]	=	data
		ls_sagmultipuerto				=	data
		li_Cliente						=	Integer(istr_mant.argumento[3]) 
		li_Planta						=	Integer(istr_mant.argumento[1])
		
		SELECT Max(defe_guides)
		INTO :ll_guides
		FROM dbo.despafrigoen 
		WHERE clie_codigo	=	:li_Cliente
		AND   plde_codigo	=	:li_Planta
		AND   defe_nturno	=	:ls_TipoPla
		AND   defe_plasag	=	:ll_Planilla;
		
		SELECT count(*),defe_glosas,defe_glosag,defe_ubisel,defe_numsel,defe_nrcont,defe_tpcont,defe_orcont,defe_patent,defe_pataco,
			sire_codigo, defe_espmul, tran_codigo, tica_codigo, defe_chofer, defe_chfrut, defe_nrosps, tpco_codigo,
			defe_totkgb, defe_taraco, defe_totvgm
		INTO :li_existe, :ls_glosa, :ls_glosag,: ls_ubisel, :ls_numsel, :ls_nrcont, :ll_tpcont, :ll_orcont, :ls_patent, :ls_pataco,
			:li_sicodigo, :li_espmul, :li_trancodigo, :li_ticacodigo, :ls_chofer, :ls_rutchofer, :ls_sagmultipuerto, :li_tipocontenedor,
			:ll_totkgb, :ll_taraco, :ll_totvgm
		FROM dbo.despafrigoen 
		WHERE clie_codigo	=	:li_Cliente
		AND   plde_codigo	=	:li_Planta
		AND   defe_nturno	=	:ls_TipoPla
		AND   defe_plasag	=	:ll_Planilla
		AND   defe_guides =  :ll_guides
		group by defe_glosas,defe_glosag,defe_ubisel,defe_numsel,defe_nrcont,defe_tpcont,defe_orcont,defe_patent,defe_pataco,
			sire_codigo, defe_espmul, tran_codigo, tica_codigo, defe_chofer, defe_chfrut, defe_nrosps, tpco_codigo,defe_totkgb, defe_taraco, defe_totvgm;
		
		If li_existe > 0 Then
			This.Object.sire_codigo[1] = li_sicodigo
			This.Object.defe_glosas[1] = ls_glosa
			This.Object.defe_ubisel[1] = ls_ubisel
			This.Object.defe_numsel[1] = ls_numsel
			This.Object.defe_nrcont[1] = ls_nrcont
			This.Object.defe_tpcont[1] = ll_tpcont
			This.Object.defe_orcont[1] = ll_orcont
			This.Object.defe_patent[1] = ls_patent
			This.Object.defe_glosag[1] = ls_glosag
			This.Object.defe_pataco[1] = ls_pataco
			This.Object.defe_espmul[1] = li_espmul
			This.Object.tran_codigo[1] = li_trancodigo
			This.Object.tica_codigo[1] = li_ticacodigo
			This.Object.defe_chofer[1] = ls_chofer
			This.Object.defe_chfrut[1] = ls_rutchofer
			This.Object.defe_nrosps[1] = ls_sagmultipuerto
			This.Object.tpco_codigo[1] = li_tipocontenedor
			This.Object.defe_totkgb[1] = ll_totkgb
			This.Object.defe_taraco[1] = ll_taraco
//			This.Object.defe_totvgm[1] = ll_totvgm
		End If	
	Else
		This.SetItem(1, 'defe_nturno', ls_null)
		This.SetItem(1, 'sire_codigo', Integer(ls_null))
		This.SetItem(1, 'defe_glosag', ls_null)
		This.SetItem(1, 'defe_espmul', Integer(ls_null))
		This.SetItem(1, 'defe_nrosps', ls_null)
		dw_2.SetTabOrder("defe_espmul",0)
		dw_2.ModIfy("defe_espmul.BackGround.Color = " + String(RGB(166,180,210)))
	End If
	
Case "tica_codigo"
	If Integer(Data) = 2 Then
		ii_blockcont = 1 
	Else
		This.SetItem(1, 'defe_nrcont', (ls_null))
		This.SetItem(1, 'defe_orcont', Integer(ls_null))
		This.SetItem(1, 'defe_setpoi', Long(ls_null))
		This.SetItem(1, 'defe_ventil', Long(ls_null))
		This.SetItem(1, 'defe_Fecfab', Date(ls_null))
		This.SetItem(1, 'defe_tipoat', Long(ls_null))
		This.SetItem(1, 'defe_tipoca', String(ls_null))
		This.SetItem(1, 'defe_totkgb', Long(ls_null))
		This.SetItem(1, 'defe_taraco', Long(ls_null))
//		This.SetItem(1, 'defe_totvgm', Long(ls_null))
		ii_blockcont = 0
	End If	
	
Case "defe_patent"
		If ib_conectado2 = True Then
			If iuo_patente.existe(data,True,sqlconec2) Then
				dw_2.Object.defe_fecing[Row] = iuo_patente.FechaIng
				dw_2.Object.defe_horing[Row] = iuo_patente.HoraIng
				dw_2.Object.defe_sucuco[Row] = iuo_patente.Sucursal
			Else	
				dw_2.Object.defe_fecing[Row] = Date(ls_null)
				dw_2.Object.defe_horing[Row] = Time(ls_null)
				dw_2.Object.defe_sucuco[Row] = Integer(ls_null)
			End If	
		End If	
	
Case "defe_especi"
	If iuo_Especie.Existe(Integer(data),True,SQLCA) Then
		istr_mant.argumento[10] = String(Data)	
		If idwc_multipuerto.Retrieve(Integer(data)) = 0 Then
			idwc_multipuerto.InsertRow(0)
		End If
	Else
		This.SetItem(row,'defe_especi',Integer(ls_null))
		Return 1
	End If
	
	Case "defe_nrcont"
		ls_digito 	= f_digito_verIfica_contene(data) 
		If ls_digito = '10' Then ls_digito = '0'
		ls_digito1 	= mid(data,11,1)
		
		If Upper(ls_digito) <> Upper(ls_digito1) Then
			MessageBox("Atención","Número Contenedor Esta Incorrecto, Digite Otro Número.")
			This.SetItem(row,'defe_nrcont',ls_null)
			Return 1
		End If		
		
	Case "defe_orcont"
		ii_blockcont = 1
	
	Case "sello"		
		ll_sello	=	Long(data)
		
		If BusCasellos(ll_sello) = False Then
			This.SetItem(Row, ls_Columna, ll_null)
			Return 1
		End If
		
	Case "trat_codigo"
		If dw_2.Object.defe_tiposa[1] = 7 OR dw_2.Object.defe_tiposa[1] = 8 OR dw_2.Object.defe_tiposa[1] = 9 Then
			If NOT IsNull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] <> '' Then
				buscadestino(dw_2.Object.embq_codigo[1],Integer(data))
			Else	
				MessageBox("Atención","Falta Selección del Embarque.")
				This.SetItem(Row, ls_Columna, Integer(ll_null))
				Return 1
			End If
			
			If Integer(il_desreferencia) <> il_destino Then
				MessageBox("Atención","Destino Seleccionado no Corresponde al Embarque.")
			End If	
		End If
		
	Case "clpr_rut"		
		is_rutclie = F_verrut(data, True)
		If is_rutclie = "" Then
			dw_2.SetItem(1, "clpr_rut", string(ll_null))
			Return 1
		Else
			If NOT iuo_clprv.existe(is_rutclie, true, sqlca) Then
				dw_2.SetItem(1, "clpr_rut", string(ll_null))
				Return 1
			Else
				dw_2.SetItem(1, "clpr_rut", is_rutclie)
				dw_2.SetItem(1, "clpr_nombre", iuo_clprv.RazonSocial)
			End If
		End If
		
	Case "defe_consol"
		If data = '0' Then dw_2.Object.defe_plades[1] = Integer(ll_null)
	
	Case "defe_frucom"	
		If data = '1' Then MessageBox("Atención","Esta Seleccionando Traspasar Fruta a Comercial.")
		
	Case "defe_chfrut"
		is_rutchofer = F_verrut(data, True)
		If is_rutchofer = "" Then
			dw_2.SetItem(1, "defe_chfrut", string(ll_null))
			Return 1
		Else
			dw_2.SetItem(1, "defe_chfrut", is_rutchofer)
			Return 1
		End If
		
	Case "tran_codigo"
		If Not iuo_Transportista.Existe(Integer(Data), True, SQLCA) Then
			This.SetItem(1, ls_columna, ll_null)
			Return 1
		End If
		
	Case "defe_traser"
		If Not iuo_TransportistaServ.Existe(Integer(Data), True, SQLCA) Then
			This.SetItem(1, ls_columna, ll_null)
			Return 1
		End If
		
End Choose

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::clicked;call super::clicked;str_mant	lstr_mant

Choose Case dwo.name
	Case "buscaembarque"
		buscaembarque()
	
	Case "buscacliente"
		buscacliente()
   		//HabilitaIngreso('clie_codigo')
			
	Case "b_termografo"	
		If isnull(dw_2.Object.defe_term01[Row]) Then
			istr_mant3.argumento[1] = ''
		Else
			istr_mant3.argumento[1] = dw_2.Object.defe_term01[Row] 
		End If
		
		If isnull(dw_2.Object.defe_term02[Row] ) Then
			istr_mant3.argumento[2] = ''
		Else
			istr_mant3.argumento[2] = dw_2.Object.defe_term02[Row] 
		End If	
		
		If  isnull(dw_2.Object.defe_term03[Row]) Then 
			istr_mant3.argumento[3] = ''	
		Else
			istr_mant3.argumento[3] = dw_2.Object.defe_term03[Row] 
		End If
		
		OpenWithParm(w_incluye_termografos, istr_mant3)
		istr_mant3 = Message.PowerObjectParm		
			
		dw_2.Object.defe_term01[Row] = istr_mant3.argumento[1]
		dw_2.Object.defe_term02[Row] = istr_mant3.argumento[2]
		dw_2.Object.defe_term03[Row] = istr_mant3.argumento[3]

	Case "b_cont"
		If ii_blockcont = 1 Then 
			lstr_mant.dw = dw_2
			OpenWithParm(w_datos_contenedor, lstr_mant)
			lstr_mant = Message.PowerObjectParm		
		End If
End Choose
end event

event dw_2::add_row;call super::add_row;Clipboard("")
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;
//IF is_rutchofer <> "" THEN
//	IF dwo.Name = "defe_chfrut" THEN
//		IF is_rutchofer <> "" THEN
//			This.SetItem(1, "defe_chfrut", String(Double(Mid(is_rutchofer, 1, 9)), "#########") + Mid(is_rutchofer, 10))
//		END IF
//	ELSE
//		This.SetItem(1, "defe_chfrut", is_rutchofer)
//	END IF
//END IF

CHOOSE CASE dwo.name
	CASE "defe_nrcont"
		cb_limpia.PostEvent(Clicked!)
		
		
	CASE "contenedor"	
		cb_limpia.PostEvent(Clicked!)
		
END CHOOSE
end event

event dw_2::ue_tecla;call super::ue_tecla;IF This.GetColumnName() = 'defe_nrcont' OR This.GetColumnName() = 'contenedor' THEN
	cb_limpia.TriggerEvent(Clicked!)
END IF	
end event

event dw_2::getfocus;call super::getfocus;IF This.GetColumnName() = 'defe_nrcont' OR This.GetColumnName() = 'contenedor' THEN
	cb_limpia.TriggerEvent(Clicked!)
END IF	
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_despafrigoen
integer x = 3141
integer y = 264
end type

event pb_nuevo::clicked;call super::clicked;ib_existe_folioD	=	False
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_despafrigoen
integer x = 3136
integer y = 492
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_despafrigoen
integer x = 3136
integer y = 708
end type

event pb_grabar::clicked;call super::clicked;//ib_existe_folioD	=	True
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_despafrigoen
integer x = 3136
integer y = 940
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_despafrigoen
integer x = 3136
integer y = 1164
end type

event pb_salir::clicked;DISCONNECT USING sqlconec2;

Close(Parent)
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_despafrigoen
integer x = 3136
integer y = 1452
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_despafrigoen
integer x = 3136
integer y = 1628
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_despafrigoen
integer x = 3141
integer y = 84
end type

event pb_buscar::clicked;call super::clicked;IF il_cierra = 1 THEN
	Pb_salir.TriggerEvent(Clicked!)
	Return
END IF	
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
dw_9.Reset()
dw_10.Reset()
dw_11.Reset()
dw_12.Reset()
end event

type dw_3 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1253
integer y = 2368
integer width = 197
integer height = 152
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_recfruprocee_trans"
end type

type dw_4 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1047
integer y = 2368
integer width = 197
integer height = 152
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_recfruproced_trans"
end type

type dw_5 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1253
integer y = 2680
integer width = 197
integer height = 152
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletencab_historia"
end type

type dw_7 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1664
integer y = 2524
integer width = 197
integer height = 152
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletencab_trans"
end type

type dw_8 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1458
integer y = 2368
integer width = 197
integer height = 152
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletfruta_trans_interplanta"
end type

type dw_9 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1253
integer y = 2212
integer width = 197
integer height = 152
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspec"
end type

type dw_10 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1458
integer y = 2212
integer width = 197
integer height = 152
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspecdet"
end type

type dw_11 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1664
integer y = 2212
integer width = 197
integer height = 152
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspec_trans"
end type

type dw_12 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1870
integer y = 2212
integer width = 197
integer height = 152
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspecdet_trans"
end type

type dw_13 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1047
integer y = 2524
integer width = 197
integer height = 152
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_archivo_saam_plano"
end type

type dw_14 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1664
integer y = 2680
integer width = 197
integer height = 152
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_caja"
end type

type dw_15 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1870
integer y = 2524
integer width = 197
integer height = 152
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_trans_interplanta"
end type

type dw_alpalletencab from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1458
integer y = 2524
integer width = 197
integer height = 152
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletencab"
end type

type dw_alpalletfruta from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1047
integer y = 2680
integer width = 197
integer height = 152
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletfruta"
end type

type dw_palletencahisto from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1664
integer y = 2368
integer width = 197
integer height = 152
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletencabhisto_inpe"
end type

type dw_palletfrutahisto from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1458
integer y = 2680
integer width = 197
integer height = 152
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletfrutahisto_inpe"
end type

type dw_archplano from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1047
integer y = 2212
integer width = 197
integer height = 152
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_arch_planos_cajasprod_despaemba"
end type

type dw_16 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1870
integer y = 2368
integer width = 197
integer height = 152
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletencab_trans"
end type

type dw_6 from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1253
integer y = 2524
integer width = 197
integer height = 152
integer taborder = 130
string title = "none"
string dataobject = "dw_mues_palletfruta_despa"
end type

type dw_inspeccion_enc from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 1870
integer y = 2680
integer width = 197
integer height = 152
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_despacho_inspecciones_enc"
end type

type dw_inspeccion_deta from datawindow within w_maed_despafrigoen
boolean visible = false
integer x = 2107
integer y = 2460
integer width = 197
integer height = 152
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_despacho_inspecciones_det"
end type

type cb_limpia from commandbutton within w_maed_despafrigoen
boolean visible = false
integer x = 3159
integer y = 1956
integer width = 402
integer height = 112
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "none"
end type

event clicked;clipboard()
end event

