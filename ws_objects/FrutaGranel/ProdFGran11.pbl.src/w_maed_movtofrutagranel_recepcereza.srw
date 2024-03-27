$PBExportHeader$w_maed_movtofrutagranel_recepcereza.srw
$PBExportComments$Recepción de Fruta Granel de Huerto
forward
global type w_maed_movtofrutagranel_recepcereza from w_mant_encab_deta_csd
end type
type cb_guia from commandbutton within w_maed_movtofrutagranel_recepcereza
end type
type dw_4 from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_spro_bins from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_9 from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_6 from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_exideta from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_exiencab from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_envases_comer from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type dw_lotescategoria from datawindow within w_maed_movtofrutagranel_recepcereza
end type
type ole_puerta from olecustomcontrol within w_maed_movtofrutagranel_recepcereza
end type
type tab_1 from tab within w_maed_movtofrutagranel_recepcereza
end type
type tp_1 from userobject within tab_1
end type
type dw_lotes from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_lotes dw_lotes
end type
type tp_2 from userobject within tab_1
end type
type dw_envrec from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envrec dw_envrec
end type
type tp_3 from userobject within tab_1
end type
type dw_envdes from uo_dw within tp_3
end type
type tp_3 from userobject within tab_1
dw_envdes dw_envdes
end type
type tab_1 from tab within w_maed_movtofrutagranel_recepcereza
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type
type str_productores_envases from structure within w_maed_movtofrutagranel_recepcereza
end type
type str_pesaje from structure within w_maed_movtofrutagranel_recepcereza
end type
type productoresxlote from structure within w_maed_movtofrutagranel_recepcereza
end type
type str_envase from structure within w_maed_movtofrutagranel_recepcereza
end type
end forward

type str_productores_envases from structure
	long		productor[]
	long		guiasii[]
	integer		tipomovto[]
	integer		numero[]
	integer		cliente[]
end type

type str_pesaje from structure
	datetime		fechahora[]
	decimal { 4 }		pesaje[]
	decimal { 4 }		total
	boolean		agrega
	boolean		modifica
	str_puertacomm		puerta
	datawindow		dw
	string		argum[]
end type

type productoresxlote from structure
	integer		prod_codigo[]
	string		prod_rut[]
	string		prod_nombre[]
	integer		lote_codigo[]
	integer		lote_espcod[]
	integer		lote_pltcod[]
end type

type str_envase from structure
	integer		tipenv
	integer		codenv
	string		calcos
	integer		cantid
end type

global type w_maed_movtofrutagranel_recepcereza from w_mant_encab_deta_csd
integer width = 4165
integer height = 2204
string title = "RECEPCION DE HUERTO"
string menuname = ""
boolean center = true
event ue_imprimir_tarja ( )
event type long ue_despuesguardar ( )
event ue_despuesborrar ( )
event type long ue_despuesguardar2 ( )
event ue_despuesborrar2 ( )
event anulamovto ( )
event anulamovto2 ( )
cb_guia cb_guia
dw_4 dw_4
dw_spro_bins dw_spro_bins
dw_9 dw_9
dw_6 dw_6
dw_exideta dw_exideta
dw_exiencab dw_exiencab
dw_exismovtodetanulos dw_exismovtodetanulos
dw_exidetaborra dw_exidetaborra
dw_envases_comer dw_envases_comer
dw_lotescategoria dw_lotescategoria
ole_puerta ole_puerta
tab_1 tab_1
end type
global w_maed_movtofrutagranel_recepcereza w_maed_movtofrutagranel_recepcereza

type prototypes

end prototypes

type variables
w_mant_deta_lotesfrutagranel_recepcereza	iw_mantencion_1
w_mant_deta_movtoenvadeta_recepfruta		iw_mantencion_2

DataWindowChild									idwc_Transp,idwc_Camion,idwc_Variedad,idwc_Predio, idwc_bins,&
														idwc_Camara,idwc_especie,idwc_especiedet,idwc_especiedet1,&
														idwc_especiedet2,idwc_planta,idwc_plantadw4,idwc_plancodw4, &
														idwc_plantadw1,idwc_Cliente,idwc_tibapa
DataWindow										dw_3, dw_5, dw_7

Str_mant												istr_mant3, istr_mant4
uo_transportista									iuo_Transport
uo_camiones										iuo_Camion
uo_especie											iuo_Especie
uo_Productores										iuo_Productor
uo_LotesCorrel										iuo_Correlativo
uo_pesoestanespe									iuo_PesoEstanEspe
uo_fechaMovto										iuo_FechaMovto
uo_tipomovtofruta									iuo_TipoMovtoFruta
uo_tipomovtofruta									iuo_TipoMovtoEnva
uo_bins												iuo_bins
uo_calicosechero 		 							iuo_calicosechero
uo_grabatablabitacora							iuo_grabatablabitacora

Transaction											sqlexi

Long     											il_NumFruta=0, il_NumEnva=0, il_lotes[], il_numeroenva, li_retorno, il_coderror
Boolean											ib_AutoCommit, ib_Salida, ib_Destare, ib_ocx, &
													ib_graba_destare, ib_ConectadoExistencia
String												is_rut, is_rutprod, is_RutProductor, is_NombreProductor, &
													is_chofer, is_correo, is_error
Integer											ii_Cantidad, ii_Productor, ii_kildec=0, ii_especie, &
													il_coneccion, il_conexiste, il_packing
DateTime										idt_FechaSistema
Decimal											id_KilosEnv
String												is_enva_tipoen[], is_enva_codigo[], is_cale_calida[], &
													is_cantidad[], is_pesone[], is_cale_nombre[]
						
Private:
str_Productores_Envases					wstr_Prod_Enva
str_pesaje              						wstr_pesaje
str_pesaje             	 					wstr_pesajeCarro
str_puertacomm		      				istr_puertacomm
end variables

forward prototypes
public subroutine determina_productoresenvase (integer ai_tipomovto)
protected function integer wf_modifica ()
public subroutine destare (boolean ab_actualiza)
public subroutine habilitaingreso (string as_columna)
public subroutine habilitaencab (boolean habilita)
public subroutine habilitagrabacion (string as_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean chequea_envasesproductor ()
public subroutine productoreslotes (ref string productores[])
public function boolean noexistechofer (string rut)
public subroutine habilitasalida ()
public subroutine actualizakiloslote ()
public subroutine buscacamion ()
public subroutine captura_totales ()
public function boolean actual_ultimo_lote ()
public function boolean noexistecliente (integer cliente)
public function boolean procesopacking ()
public function boolean ordenproceso ()
public subroutine asignapeso ()
public function boolean existedocproceso (integer ai_planta, long al_numero)
public function boolean existemovtoproceso (integer ai_planta, integer ai_tipomovto, integer ai_tipodocto, long al_proceso)
public subroutine f_membreteds (datastore ds_1)
public function boolean conexionexistencia ()
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, long al_numero, integer ai_cliente)
public subroutine updatemovtogranpesa ()
public function boolean chequea_binscontraenvases ()
public subroutine cargamovenv ()
public subroutine cargaenvases ()
public function boolean packingproductor (long al_productor)
public subroutine set_tarjas (integer cliente, long planta)
public subroutine wf_bloqueacolumnas (boolean ab_bloquea)
public function boolean datos_correo ()
end prototypes

event ue_imprimir_tarja();SetPointer(HourGlass!)
Datawindowchild  	ldwc_lotes
DataStore 		  	lds_Informe 
Long				  	fila,ll_fila, respuesta
String 	      		n_lote, tmp, command
Integer 				li_imprimir, li_bultos, li_tarjas

IF NOT gstr_paramplanta.binsabins AND NOT gstr_paramplanta.palletdebins THEN Return

li_imprimir 	=	MessageBox("Lote", "¿ Desea Imprimir una tarja para cada Bulto del Lote ?~r"+&
												"Presione No para imprimir solo una tarja~r"+&
												"o Cancelar para no imprimir tarjas", Question!, YesNoCancel!, 1)

IF li_imprimir <> 3 THEN
	
	lds_informe	= Create DataStore
	lds_Informe.DataObject = "dw_info_lotesfrutagranel_recepcion_grand"
	
	DO 
		lds_Informe.SetTransObject(Sqlca)
		ll_fila	= lds_Informe.Retrieve(dw_2.Object.plde_codigo[dw_2.GetRow()],Integer(istr_mant.argumento[10]),&
										  1,dw_2.Object.mfge_numero[dw_2.GetRow()])
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		ELSE
			FOR ll_fila = 1 TO tab_1.tp_1.dw_lotes.RowCount()
		 
				n_lote =  String(tab_1.tp_1.dw_lotes.object.compute_2[ll_fila])
			 
				IF li_imprimir = 1 THEN
					li_bultos	=	tab_1.tp_1.dw_lotes.Object.lote_totbul[ll_fila]
				ELSE
					li_bultos	=	1
				END IF
				
				lds_Informe.SetFilter("compute_2 = '"+ n_lote +" '")
				lds_Informe.Filter()
				
				FOR li_tarjas = 1 TO li_bultos
					lds_Informe.Print()
				NEXT	
				
			NEXT
			SetPointer(Arrow!)
		END IF
	LOOP WHILE respuesta = 1

END IF
	
	
Destroy lds_informe

li_imprimir 	=	MessageBox("Lote", "¿ Desea Imprimir Anexo de Pesaje del Lote ?~r", Question!, YesNo!, 1)

IF li_imprimir = 1 THEN
	lds_informe	= Create DataStore
	
	IF NOT gstr_paramplanta.palletdebins THEN
		lds_Informe.DataObject = "dw_info_anexo_pesaje_recepcion_granel"
	ELSE
		lds_Informe.DataObject = "dw_info_anexo_pesaje_recepcion_granel"
	END IF
	
	lds_Informe.SetTransObject(Sqlca)
	ll_fila	= lds_Informe.Retrieve(dw_2.Object.plde_codigo[dw_2.GetRow()],&
										  1,dw_2.Object.mfge_numero[dw_2.GetRow()],&
										  Integer(istr_mant.argumento[10]))
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSE
		f_membreteds(lds_Informe)
		lds_Informe.Print()
	END IF
END IF
end event

event type long ue_despuesguardar();integer	li_bodega, li_fila, li_cliente, li_enva_codigo, li_enva_tipoen, li_packing,&
			li_bodevirtual, li_bodzonal, li_planta, li_tipomov, li_lote, li_categoria, li_envase,li_tipoenva,li_bodecomercial, li_devcorreo
Long 		ll_fila, ll_numero, li_secuencia = 1, ll_numnuevoini, ll_numnuevofin, ll_count, ll_docrel, &
			ll_recepcion, ll_filote, ll_filas, ll_Filexiste, ll_filaenva, ll_cantidad, ll_secuencia2
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_calilote, ls_correozonal, ls_correo, ls_texto, ls_asunto, ls_error, serrormsg
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_fecha	

IF ib_Conectadoexistencia = TRUE THEN
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	li_cliente 	= dw_2.Object.clie_codigo[1]
	li_planta 	= dw_2.Object.plde_codigo[1] 
	li_tipomov	= dw_2.Object.tpmv_codigo[1]
	ll_recepcion = dw_2.Object.mfge_numero[1]
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
		
	FOR ll_filote = 1 TO dw_lotescategoria.RowCount()
		
		li_lote			= dw_lotescategoria.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_lotescategoria.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
//			IF li_categoria = 101 THEN
//				//ll_secuencia2 = li_secuencia + 1
//				li_secuencia = 1 
//			END IF
			
			IF li_secuencia = 1 THEN
								
				IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
					Message.DoubleParm = -1
					li_retorno = 2
					is_error = 'Error en Parámetros de Existencia'
					RETURN 2
				ELSE
					li_bodevirtual 	= luo_existencia.bodvirtual
					li_bodzonal			= luo_existencia.bodzonal
					li_bodecomercial 	= luo_existencia.bodcomercial
				END IF	
							
				ll_docrel = dw_2.Object.mfge_numero[1]
				
//				IF li_categoria = 101 THEN
//					li_bodevirtual 	= luo_existencia.bodcomercial				
//				END IF	
				
				IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
					Message.DoubleParm = -1
					
					ll_docrel 	= dw_2.Object.mfge_numero[1]
					ld_fecha		= dw_2.Object.mfge_fecmov[1]
					
					IF luo_existencia.maximo_porfecha(1,li_bodzonal,ll_docrel,1,ld_fecha,True,sqlexi) THEN
						ll_numero = luo_existencia.numero
					END IF
					
					IF isnull(ll_numero) THEN
						ll_numero = 0
					END IF	
							
					IF luo_existencia.bodega_zonal(li_bodzonal,sqlexi,True) THEN
						ls_correozonal = luo_existencia.correozonal
					END IF	
					
					ls_correo = luo_existencia.correo
					
					IF ll_numero = 0 THEN
						ls_texto	 = "No es posible actualizar movto. en Existencia, por modificación anterior al mes de proceso(No presenta movimiento en bodegas)"
					ELSE	
						ls_texto	 = "No es posible actualizar movto. en Existencia Nº "+String(ll_numero)+"; por modificación anterior al mes de proceso"
					END IF
					
					ls_asunto = "Modifica Recepción fruta granel, Movto. Nº "+String(ll_docrel)
					//li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaGranel@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
//					
//					IF (li_devcorreo<0) THEN
//						messagebox("Error No" + string(li_devcorreo),sErrorMsg)
//					END IF
					li_retorno = 2
					is_error = ls_texto
					RETURN 2
				END IF	
				
				luo_existencia.existeregistro(ll_docrel,li_bodzonal,1,1,True,sqlexi,dw_2.Object.mfge_fecmov[1])
				
				IF luo_existencia.count = 0 THEN
					IF Not luo_existencia.correlativobode(1,li_bodzonal,li_bodzonal,True,sqlexi) THEN
						Message.DoubleParm = -1
						li_retorno = 2
						is_error = 'Error en Correlativos de Bodega'
						RETURN 2
					ELSE
						ll_numero = luo_existencia.numero
					END IF	
							
					IF isnull(ll_numero) THEN
						ll_numnuevoini = Long(String(li_bodzonal)+''+'0001')
						ll_numnuevofin = Long(String(li_bodzonal)+''+'9999')
						
						INSERT INTO dba.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
						VALUES(:li_bodzonal,1,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
						USING sqlexi;
						
						IF sqlexi.SQLCode = -1 THEN
							F_ErrorBaseDatos(sqlexi,"Correlbode")
							Message.DoubleParm = -1
							sqlexi.AutoCommit	=	False
							RETURN 2
						END IF
						ll_numero = ll_numnuevoini - 1
					END IF	
					ll_numero = ll_numero + 1
					
					ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
							
					IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
						Message.DoubleParm = -1
						li_retorno = 2
						is_error = 'Error en Tabla Cliente en existencia, es Posible que Cliente no Exista '+ls_productor
						Return 2
						ls_productor = luo_existencia.prod
					END IF
										
					IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
						ls_productor = luo_existencia.prdgen
					END IF	
					
					IF isnull(dw_3.Object.lote_prdpak[1]) THEN
						IF NOT packingproductor(long(ls_productor)) THEN
							il_packing = dw_3.Object.lote_prdpak[1]
							
							IF luo_existencia.bodega_zonal(li_bodzonal,sqlexi,True) THEN
								ls_correo = luo_existencia.correozonal
							END IF	
							
							IF isnull(il_packing) OR il_packing = 0 THEN
								ls_texto	 = "Movto. Recepción Nº "+ string(ll_numero) +", NO Genera Movto. en Existencia. Debe Realizar Mantención Productor-Packing."
								ls_asunto = "Falta Packing en Recepción"	
							ELSE	
								ls_texto	 = "Falta Código de Packing Nº " + String(il_packing) +", Debe Realizar Mantención Productor-Packing"
								ls_asunto = "Falta Packing en Tabla"
							END IF						
													
//							li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaGranel@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
							
							IF isnull(il_packing) OR il_packing = 0 THEN
								li_retorno = 2
								is_error = 'Packing NO Puede ser Nulo o Cero'
								Return 2
							END IF	
						END IF	
					ELSE
						il_packing = dw_3.Object.lote_prdpak[1]
					END IF
					
					luo_existencia.bodega_administradora(il_packing,True, sqlexi)
					
					IF luo_existencia.bodeAdmi <> luo_existencia.bodzonal THEN
						//Pato pide que se haga asi 2014/12/15
						il_packing = luo_existencia.bodeAdmi
						
						SELECT isnull(plde_conpro,0)
						INTO :il_coneccion 
						FROM dbo.plantadesp
						WHERE plde_codigo = :il_packing;
						
						IF il_coneccion = 0 THEN
							il_coneccion = 95
						END IF	
						
					ELSE
						SELECT 	cone_codigo
						INTO   	:il_coneccion
						FROM dbo.clientesprod
						WHERE clie_codigo = :li_Cliente;
					END IF
					
					dw_exiencab.Object.mden_tipdoc[1] = 1
					dw_exiencab.Object.mden_numero[1] = ll_numero 
					dw_exiencab.Object.tpdo_codigo[1] = 3
					dw_exiencab.Object.mden_fecmov[1] = dw_2.Object.mfge_fecmov[1]
					dw_exiencab.Object.tpmv_tipomv[1] = 1
					dw_exiencab.Object.tpmv_codigo[1] = 16
					dw_exiencab.Object.mden_tipana[1] = 3
					dw_exiencab.Object.mden_estaci[1] = gstr_us.computador
					dw_exiencab.Object.mden_fecdig[1] = Date(Today())
					dw_exiencab.Object.mden_hordig[1] = Time(Now())
					
					dw_exiencab.Object.mden_gdprod[1] = dw_4.Object.meen_guisii[1]
					
					//IF li_categoria <> 101 THEN
						dw_exiencab.Object.bode_codigo[1] = li_bodzonal
					//ELSE
					//	dw_exiencab.Object.bode_codigo[1] = luo_existencia.bodcomercial
					//END IF 
					
					dw_exiencab.Object.mden_bodest[1] = il_packing
					dw_exiencab.Object.clpr_rut[1]	 = ls_productor
					dw_exiencab.Object.mden_docrel[1] = dw_2.Object.mfge_numero[1]
					dw_exiencab.Object.mden_fecdre[1] = dw_2.Object.mfge_fecmov[1]
					dw_exiencab.Object.mden_observ[1] = 'Recepción Envases Granel'
					dw_exiencab.Object.mden_estado[1] = 1
					dw_exiencab.Object.mden_pcopda[1] = 1
				ELSE
					luo_existencia.numero_actual(ll_docrel,li_bodzonal,1,1,True,sqlexi,dw_2.Object.mfge_fecmov[1])
					ll_numero = luo_existencia.numeroactual
				END IF									
			END IF							
									
			li_enva_codigo = dw_envases_comer.Object.enva_codigo[ll_filaenva]
			li_enva_tipoen = dw_envases_comer.Object.enva_tipoen[ll_filaenva]
			ls_calidad		= dw_envases_comer.Object.cale_calida[ll_filaenva]
			ll_cantidad		= dw_envases_comer.Object.cantidad[ll_filaenva]
													
			ll_fila	=	dw_exideta.InsertRow(0)
									
			IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
				Message.DoubleParm = -1
				li_retorno = 2
				is_error = 'Problema con envases'
				RETURN 2
			ELSE
				ls_item_codigo = iuo_calicosechero.item 
			END IF	
			
			IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
				ls_item_codigo = luo_existencia.itgene
			END IF
							
			dw_exideta.Object.mden_tipdoc[ll_fila] = 1
			dw_exideta.Object.mden_numero[ll_fila] = ll_numero
			ll_secuencia2 ++
//			IF li_categoria = 101 THEN
//				dw_exideta.Object.mdde_secuen[ll_fila] = ll_secuencia2 
//			ELSE
				dw_exideta.Object.mdde_secuen[ll_fila] = li_secuencia 
//			END IF
			dw_exideta.Object.tpmv_tipomv[ll_fila] = 1 
			dw_exideta.Object.tpmv_codigo[ll_fila] = 16
			dw_exideta.Object.item_codigo[ll_fila] = ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_fila] = ''
			dw_exideta.Object.mdde_fecmov[ll_fila] = dw_2.Object.mfge_fecmov[1]
			dw_exideta.Object.bode_codigo[ll_fila] = li_bodzonal
			dw_exideta.Object.mdde_cantid[ll_fila] = ll_cantidad
						
			li_secuencia = li_secuencia + 1
				
		NEXT		
		IF dw_exiencab.Rowcount() > 0 OR li_secuencia > 1 THEN
			lb_AutoCommit		=	sqlexi.AutoCommit
			sqlexi.AutoCommit	=	False
		
			IF dw_exiencab.Update(True, False) = 1 THEN
				IF dw_exideta.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlexi.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlexi, This.Title)
						
						RollBack;
						li_retorno = 2	
						//Messagebox("Existencia","Grabación de Datos NO se realizó")
					ELSE
						li_retorno = 1						
						dw_exiencab.ResetUpdate()
						dw_exideta.ResetUpdate()
					//	Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
						
					END IF
				ELSE
									
					RollBack;
					li_retorno = 2
				//	Messagebox("Existencia","Grabación de Datos NO se realizó")
				END IF
			ELSE
				F_ErrorBaseDatos(sqlexi, This.Title)
			
				RollBack;
				li_retorno = 2	
			//	Messagebox("Existencia","Grabación de Datos NO se realizó")
			END IF
				
			dw_exiencab.Reset()
			dw_exideta.Reset()
		END IF
	NEXT	
	
	pb_grabar.Enabled = False
	dw_exiencab.Reset()
	dw_exideta.Reset()
	sqlexi.AutoCommit	=	True
   Return li_retorno
END IF
end event

event ue_despuesborrar();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
date	ld_fecha		
String	ls_productor

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
	//	sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual 	= luo_existencia.bodvirtual
		li_bodzonal			= luo_existencia.bodzonal
		li_bodecomercial 	= luo_existencia.bodcomercial
	END IF	
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
	
	ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
							
	IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
		ls_productor = luo_existencia.prod
	END IF
	
	IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
		ls_productor = luo_existencia.prdgen
	END IF	
	
	IF NOT packingproductor(long(ls_productor)) THEN
		il_packing = dw_3.Object.lote_prdpak[1]
	END IF	
	
	FOR ll_filote = 1 TO dw_3.RowCount()
		
		li_lote			= dw_3.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_3.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
		
			il_numeroenva = dw_2.Object.mfge_numero[1]
			ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
			IF NOT luo_existencia.maximo_porfecha(1,li_bodzonal,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			ELSE
				ll_numero = luo_existencia.numero
			END IF
					
			IF NOT isnull(ll_numero) THEN
				//Return
							
				IF NOT luo_existencia.actualizaexistencia(2,1,li_bodzonal,ll_numero,il_numeroenva,True,sqlexi) THEN
					Message.DoubleParm = -1
					Return
				END IF	
				
				dw_exidetaborra.Retrieve(1,ll_numero)
				
				FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
					ll_nueva = dw_exismovtodetanulos.InsertRow(0)
					
					dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
					dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
					dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
					dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
					dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
					dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
			
				NEXT
				
				DELETE FROM dba.exismovtodeta 
					WHERE	mden_numero = :ll_numero
					AND	mden_tipdoc = 1
					AND	bode_codigo = :li_bodzonal
					USING sqlexi;
				
				IF sqlexi.SQLCode = -1 THEN
					F_ErrorBaseDatos(sqlexi,"exismovtodeta")
					Message.DoubleParm = -1
					sqlexi.AutoCommit	=	ib_AutoCommit
					RETURN 
				END IF
				
				IF dw_exismovtodetanulos.Rowcount() > 0 THEN
					lb_AutoCommit		=	sqlexi.AutoCommit
					sqlexi.AutoCommit	=	False
					
					IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
						
						dw_exismovtodetanulos.ResetUpdate()
			
					ELSE
						F_ErrorBaseDatos(sqlexi, This.Title)
							
						RollBack;
						Return
					END IF
					
					sqlexi.AutoCommit	=	lb_AutoCommit
				
				END IF
			END IF
		NEXT	
	NEXT
END IF
end event

event type long ue_despuesguardar2();integer	li_bodega, li_fila, li_cliente, li_enva_codigo, li_enva_tipoen, &
			li_bodevirtual, li_bodzonal, li_planta, li_tipomov, li_lote, li_categoria, li_envase,li_tipoenva,li_bodecomercial, li_devcorreo
Long 		ll_fila, ll_numero, li_secuencia = 1, ll_numnuevoini, ll_numnuevofin, ll_count, ll_docrel, &
			ll_recepcion, ll_filote, ll_filas, ll_Filexiste, ll_filaenva, ll_cantidad, ll_secuencia2
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_calilote, ls_correozonal, ls_correo, ls_texto, ls_asunto, ls_error, serrormsg
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_fecha

IF ib_Conectadoexistencia = TRUE THEN
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	li_cliente 	= dw_2.Object.clie_codigo[1]
	li_planta 	= dw_2.Object.plde_codigo[1] 
	li_tipomov	= dw_2.Object.tpmv_codigo[1]
	ll_recepcion = dw_2.Object.mfge_numero[1]
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
		
	FOR ll_filote = 1 TO dw_lotescategoria.RowCount()
		
		li_lote			= dw_lotescategoria.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_lotescategoria.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
//			IF li_categoria = 101 THEN
//				//ll_secuencia2 = li_secuencia + 1
//				li_secuencia = 1 
//			END IF
			
			IF li_secuencia = 1 THEN
								
				IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
					Message.DoubleParm = -1
					li_retorno = 2
					is_error = 'Error en Parámetros de Existencia'
					RETURN 2
				ELSE
					li_bodevirtual 	= luo_existencia.bodvirtual
					li_bodzonal			= luo_existencia.bodzonal
					li_bodecomercial 	= luo_existencia.bodcomercial
				END IF	
							
				ll_docrel = dw_2.Object.mfge_numero[1]
				
//				IF li_categoria = 101 THEN
//					li_bodevirtual 	= luo_existencia.bodcomercial				
//				END IF	
				
				IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
					Message.DoubleParm = -1
					
					ll_docrel = dw_2.Object.mfge_numero[1]
					ld_fecha		= dw_2.Object.mfge_fecmov[1]
				
					IF luo_existencia.maximo_porfecha(3,il_packing,ll_docrel,1,ld_fecha,True,sqlexi) THEN
						ll_numero = luo_existencia.numero
					END IF
					
					IF isnull(ll_numero) THEN
						ll_numero = 0
					END IF	
							
					IF luo_existencia.bodega_zonal(il_packing,sqlexi,True) THEN
						ls_correozonal = luo_existencia.correozonal
					END IF	
					
					ls_correo = luo_existencia.correo
					
					IF ll_numero = 0 THEN
						ls_texto	 = "No es posible actualizar movto. en Existencia, por modificación anterior al mes de proceso(No presenta movimiento en bodegas)"
					ELSE	
						ls_texto	 = "No es posible actualizar movto. en Existencia Nº "+String(ll_numero)+"; por modificación anterior al mes de proceso"
					END IF
					
					ls_asunto = "Modifica Recepción fruta granel, Movto. Nº "+String(ll_docrel)
				//	li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaGranel@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
//					
//					IF (li_devcorreo<0) THEN
//						messagebox("Error No" + string(li_devcorreo),sErrorMsg)
//					END IF
					li_retorno = 2
					is_error = ls_texto
					RETURN 1
				END IF	
				
				luo_existencia.existeregistro(ll_docrel,il_packing,1,3,True,sqlexi,dw_2.Object.mfge_fecmov[1])
				
				IF luo_existencia.count = 0 THEN
					IF Not luo_existencia.correlativobode(3,il_packing,li_bodzonal,True,sqlexi) THEN
						Message.DoubleParm = -1
						li_retorno = 2
						is_error = 'Error en Tabla Correlativo Bodega'
						RETURN 2
					ELSE
						ll_numero = luo_existencia.numero
					END IF	
							
					IF isnull(ll_numero) THEN
						ll_numnuevoini = Long(String(il_packing)+''+'0001')
						ll_numnuevofin = Long(String(il_packing)+''+'9999')
						
						INSERT INTO dba.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
						VALUES(:il_packing,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
						USING sqlexi;
						
						IF sqlexi.SQLCode = -1 THEN
							F_ErrorBaseDatos(sqlexi,"Correlbode")
							Message.DoubleParm = -1
							sqlexi.AutoCommit	=	ib_AutoCommit
							li_retorno = 2
							is_error = 'Problema con Correlativos de bodega'
							RETURN 2
						END IF
						ll_numero = ll_numnuevoini - 1
					END IF	
					ll_numero = ll_numero + 1
					
					ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
							
					IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
						Message.DoubleParm = -1
						ls_productor = luo_existencia.prod
						li_retorno = 2
						is_error = 'Error en Tabla Cliente en existencia, es Posible que Cliente no Exista '+ls_productor
						RETURN 2
					END IF
					
					IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
						ls_productor = luo_existencia.prdgen
					END IF	
					
					dw_exiencab.Object.mden_tipdoc[1] = 3
					dw_exiencab.Object.mden_numero[1] = ll_numero 
					dw_exiencab.Object.tpdo_codigo[1] = 3
					dw_exiencab.Object.mden_fecmov[1] = dw_2.Object.mfge_fecmov[1]
					dw_exiencab.Object.tpmv_tipomv[1] = 2
					dw_exiencab.Object.tpmv_codigo[1] = 16
					dw_exiencab.Object.mden_tipana[1] = 3
					dw_exiencab.Object.mden_estaci[1] = gstr_us.computador
					dw_exiencab.Object.mden_fecdig[1] = Date(Today())
					dw_exiencab.Object.mden_hordig[1] = Time(Now())
					
					//IF li_categoria <> 101 THEN
						dw_exiencab.Object.bode_codigo[1] = il_packing
					//ELSE
					//	dw_exiencab.Object.bode_codigo[1] = luo_existencia.bodcomercial
					//END IF 
					
					dw_exiencab.Object.mden_bodest[1] = li_bodzonal
					dw_exiencab.Object.clpr_rut[1]	 = ls_productor
					dw_exiencab.Object.mden_gdprod[1] = dw_4.Object.meen_guisii[1]
					dw_exiencab.Object.mden_docrel[1] = dw_2.Object.mfge_numero[1]
					dw_exiencab.Object.mden_fecdre[1] = dw_2.Object.mfge_fecmov[1]
					dw_exiencab.Object.mden_observ[1] = 'Recepción Envases Granel'
					dw_exiencab.Object.mden_estado[1] = 1
					dw_exiencab.Object.mden_pcopda[1] = 1
				ELSE
					luo_existencia.numero_actual(ll_docrel,il_packing,1,3,True,sqlexi,dw_2.Object.mfge_fecmov[1])
					ll_numero = luo_existencia.numeroactual
				END IF									
			END IF							
									
			li_enva_codigo = dw_envases_comer.Object.enva_codigo[ll_filaenva]
			li_enva_tipoen = dw_envases_comer.Object.enva_tipoen[ll_filaenva]
			ls_calidad		= dw_envases_comer.Object.cale_calida[ll_filaenva]
			ll_cantidad		= dw_envases_comer.Object.cantidad[ll_filaenva]
													
			ll_fila	=	dw_exideta.InsertRow(0)
									
			IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
				Message.DoubleParm = -1
				li_retorno = 2
				is_error = 'Problema con envases'
				RETURN 2
			ELSE
				ls_item_codigo = iuo_calicosechero.item 
			END IF	
			
			IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
				ls_item_codigo = luo_existencia.itgene
			END IF
							
			dw_exideta.Object.mden_tipdoc[ll_fila] = 3
			dw_exideta.Object.mden_numero[ll_fila] = ll_numero
			ll_secuencia2 ++
//			IF li_categoria = 101 THEN
//				dw_exideta.Object.mdde_secuen[ll_fila] = ll_secuencia2 
//			ELSE
				dw_exideta.Object.mdde_secuen[ll_fila] = li_secuencia 
//			END IF
			dw_exideta.Object.tpmv_tipomv[ll_fila] = 2 
			dw_exideta.Object.tpmv_codigo[ll_fila] = 16 
			dw_exideta.Object.item_codigo[ll_fila] = ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_fila] = ''
			dw_exideta.Object.mdde_fecmov[ll_fila] = dw_2.Object.mfge_fecmov[1]
			dw_exideta.Object.bode_codigo[ll_fila] = il_packing
			dw_exideta.Object.mdde_cantid[ll_fila] = ll_cantidad
						
			li_secuencia = li_secuencia + 1
				
		NEXT		
		
		IF dw_exiencab.Rowcount() > 0 OR li_secuencia > 1 THEN
			lb_AutoCommit		=	sqlexi.AutoCommit
			sqlexi.AutoCommit	=	False
		
			IF dw_exiencab.Update(True, False) = 1 THEN
				IF dw_exideta.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlexi.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlexi, This.Title)
						
						RollBack;
						li_retorno = 2
						//Messagebox("Existencia","Grabación de Datos NO se realizó")
					ELSE
						lb_Retorno	=	True
						
						dw_exiencab.ResetUpdate()
						dw_exideta.ResetUpdate()
						//Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
						li_retorno = 1
					END IF
				ELSE
					F_ErrorBaseDatos(sqlexi, This.Title)
					
					RollBack;
					//Messagebox("Existencia","Grabación de Datos NO se realizó")
					li_retorno = 2
				END IF
			ELSE
				F_ErrorBaseDatos(sqlexi, This.Title)
				
				RollBack;
				//Messagebox("Existencia","Grabación de Datos NO se realizó")
				li_retorno = 2
			END IF
				
			dw_exiencab.Reset()
			dw_exideta.Reset()
			
		END IF
	NEXT	
	
	pb_grabar.Enabled = False
	dw_exiencab.Reset()
	dw_exideta.Reset()
	sqlexi.AutoCommit	=	lb_AutoCommit
	
	Return li_retorno
	
END IF


end event

event ue_despuesborrar2();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
Date	ld_fecha		

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
	//	sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual 	= luo_existencia.bodvirtual
		li_bodzonal			= luo_existencia.bodzonal
		li_bodecomercial 	= luo_existencia.bodcomercial
	END IF	
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
	
	FOR ll_filote = 1 TO dw_3.RowCount()
		
		li_lote			= dw_3.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_3.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
		
//			IF li_categoria = 101 THEN
//				li_bodevirtual 	= luo_existencia.bodcomercial				
//			END IF
				
			il_numeroenva = dw_2.Object.mfge_numero[1]
			ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
			IF NOT luo_existencia.maximo_porfecha(3,il_packing,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			ELSE
				ll_numero = luo_existencia.numero
			END IF
					
			IF NOT isnull(ll_numero) THEN
					//Return
							
				IF NOT luo_existencia.actualizaexistencia(2,3,il_packing,ll_numero,il_numeroenva,True,sqlexi) THEN
					Message.DoubleParm = -1
					Return
				END IF	
				
				dw_exidetaborra.Retrieve(3,ll_numero)
				
				FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
					ll_nueva = dw_exismovtodetanulos.InsertRow(0)
					
					dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
					dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
					dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
					dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
					dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
					dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
			
				NEXT
				
				DELETE FROM dba.exismovtodeta 
					WHERE	mden_numero = :ll_numero
					AND	mden_tipdoc = 3
					AND	bode_codigo = :il_packing
					USING sqlexi;
				
				IF sqlexi.SQLCode = -1 THEN
					F_ErrorBaseDatos(sqlexi,"exismovtodeta")
					Message.DoubleParm = -1
					sqlexi.AutoCommit	=	ib_AutoCommit
					RETURN 
				END IF
				
				IF dw_exismovtodetanulos.Rowcount() > 0 THEN
					lb_AutoCommit		=	sqlexi.AutoCommit
					sqlexi.AutoCommit	=	False
					
					IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
						
						dw_exismovtodetanulos.ResetUpdate()
			
					ELSE
						F_ErrorBaseDatos(sqlexi, This.Title)
							
						RollBack;
						Return
					END IF
					
					sqlexi.AutoCommit	=	lb_AutoCommit
				
				END IF
			END IF
		NEXT	
	NEXT
	//DISCONNECT USING sqlexi;
	//ib_Conectadoexistencia = False
END IF






























//Long 		ll_numero, respuesta, ll_fila, ll_nueva
//Boolean 	lb_AutoCommit
//Integer	li_bodecomercial
//
//IF ib_Conectadoexistencia = TRUE THEN
//	
//	SELECT DISTINCT expa_pacfri, expa_bodega, expa_pakcom
//	INTO :gi_bodvirtual, :gi_bodzonal, :li_bodecomercial
//	FROM dbo.existeparam
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"existeparam")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		Return
//	END IF
//	
//	SELECT max(mden_numero)
//	INTO :ll_numero
//	FROM dbo.exismovtoenca
//	WHERE mden_tipdoc = 1
//	AND bode_codigo = :gi_bodvirtual
//	AND mden_bodest = :gi_bodzonal
//	AND mden_docrel = :il_numeroenva
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		Return
//	END IF
//	
//	IF isnull(ll_numero) THEN
//		Return
//	END IF	
//	
//	UPDATE dbo.exismovtoenca SET
//	mden_estado = 2
//	WHERE mden_tipdoc = 1
//	AND bode_codigo = :gi_bodvirtual
//	AND mden_bodest = :gi_bodzonal
//	AND mden_numero = :ll_numero
//	AND mden_docrel = :il_numeroenva
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		RETURN 
//	END IF
//	
//	dw_exidetaborra.Retrieve(1,ll_numero)
//	
//	FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
//		ll_nueva = dw_exismovtodetanulos.InsertRow(0)
//		
//		dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
//		dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
//		dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
//		dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
//		dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
//		dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
//		//dw_exismovtodetanulos.Object.mdde_gratri[ll_nueva] = dw_exidetaborra.Object.mdde_codbor[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco1[ll_nueva] = dw_exidetaborra.Object.mdde_codbo1[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco2[ll_nueva] = dw_exidetaborra.Object.mdde_codbo2[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco3[ll_nueva] = dw_exidetaborra.Object.mdde_codbo3[ll_fila]  
//	NEXT	
//	
//	DELETE FROM dbo.exismovtodeta 
//	WHERE	mden_tipdoc = 1 
//	AND	mden_numero = :ll_numero
//	AND	bode_codigo = :gi_bodvirtual
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtodeta")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		RETURN 
//	END IF
//	
//	IF dw_exismovtodetanulos.Rowcount() > 0 THEN
//		lb_AutoCommit		=	sqlexi.AutoCommit
//		sqlexi.AutoCommit	=	False
//		
//		IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
//			
//			dw_exismovtodetanulos.ResetUpdate()
//
//		ELSE
//			F_ErrorBaseDatos(sqlexi, This.Title)
//				
//			RollBack;
//			Return
//		END IF
//		
//		sqlexi.AutoCommit	=	lb_AutoCommit
//	
//	END IF
//	
//	dw_exidetaborra.Reset()
//	dw_exismovtodetanulos.Reset()
//	DISCONNECT USING sqlexi;
//	
//END IF
//
//
end event

event anulamovto();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
date	ld_fecha		
String	ls_productor

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
	//	sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual 	= luo_existencia.bodvirtual
		li_bodzonal			= luo_existencia.bodzonal
		li_bodecomercial 	= luo_existencia.bodcomercial
	END IF	
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
	
	ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
							
	IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
		ls_productor = luo_existencia.prod
	END IF
	
	IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
		ls_productor = luo_existencia.prdgen
	END IF	
	
	IF isnull(dw_3.Object.lote_prdpak[1]) THEN
					
		IF NOT packingproductor(long(ls_productor)) THEN
			il_packing = dw_3.Object.lote_prdpak[1]
			
			IF luo_existencia.bodega_zonal(li_bodzonal,sqlexi,True) THEN
				
			END IF	
			
		END IF	
	ELSE
		il_packing = dw_3.Object.lote_prdpak[1]	
	END IF	
	
	luo_existencia.bodega_administradora(il_packing,True, sqlexi)	
	
	IF luo_existencia.bodeAdmi <> luo_existencia.bodzonal THEN
						
		SELECT isnull(plde_conpro,0)
		INTO :il_coneccion 
		FROM "dbo"."plantadesp" 
		WHERE plde_codigo = :il_packing;
		
		IF il_coneccion = 0 THEN
			il_coneccion = 95
		END IF	
	ELSE
		SELECT 	cone_codigo
		INTO   	:il_coneccion
		FROM dbo.clientesprod
		WHERE clie_codigo = :li_Cliente;
	END IF
	
	FOR ll_filote = 1 TO dw_3.RowCount()
		
		li_lote			= dw_3.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_3.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
		
//			IF li_categoria = 101 THEN
//				li_bodevirtual 	= luo_existencia.bodcomercial				
//			END IF
				
			il_numeroenva = dw_2.Object.mfge_numero[1]
			ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
			IF NOT luo_existencia.maximo_porfecha(1,li_bodzonal,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			ELSE
				ll_numero = luo_existencia.numero
			END IF
					
			IF NOT isnull(ll_numero) THEN
				//Return
						
			IF NOT luo_existencia.actualizaexistencia(2,1,li_bodzonal,ll_numero,il_numeroenva,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			END IF	
			
			dw_exidetaborra.Retrieve(1,ll_numero)
			
			FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
				ll_nueva = dw_exismovtodetanulos.InsertRow(0)
				
				dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
				dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
				dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
				dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
				dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
				dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
		
			NEXT
			
			DELETE FROM dbo.exismovtodeta 
				WHERE	mden_numero = :ll_numero
				AND	mden_tipdoc = 1
				AND	bode_codigo = :li_bodzonal
				USING sqlexi;
			
			IF sqlexi.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlexi,"exismovtodeta")
				Message.DoubleParm = -1
				sqlexi.AutoCommit	=	ib_AutoCommit
				RETURN 
			END IF
			
			IF dw_exismovtodetanulos.Rowcount() > 0 THEN
				lb_AutoCommit		=	sqlexi.AutoCommit
				sqlexi.AutoCommit	=	False
				
				IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
					
					dw_exismovtodetanulos.ResetUpdate()
		
				ELSE
					F_ErrorBaseDatos(sqlexi, This.Title)
						
					RollBack;
					Return
				END IF
				
				sqlexi.AutoCommit	=	lb_AutoCommit
			
			END IF
		
//			IF NOT luo_existencia.nummaxpbodega(1,li_bodzonal,li_bodevirtual,il_numeroenva,True,sqlexi) THEN
//				F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//				Message.DoubleParm = -1
//				Return
//			ELSE	
//				ll_numero = luo_existencia.numero
//			END IF	
//		
//			IF isnull(ll_numero) THEN
//				Return
//			END IF	
//				
//			UPDATE dbo.exismovtoenca SET
//				mden_estado = 2
//				WHERE mden_tipdoc = 1
//				AND bode_codigo = :li_bodzonal
//				AND mden_numero = :ll_numero
//				AND mden_docrel = :il_numeroenva
//				AND mden_estado = 1
//				USING sqlexi;
//				
//			IF sqlexi.SQLCode = -1 THEN
//				F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//				Message.DoubleParm = -1
//				sqlexi.AutoCommit	=	ib_AutoCommit
//				Return
//			END IF	
//			
//			dw_exidetaborra.Retrieve(1,ll_numero)
//			
//			FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
//				ll_nueva = dw_exismovtodetanulos.InsertRow(0)
//				
//				dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
//				dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
//				dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
//				dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
//				dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
//				dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
//			
//			NEXT
//			
//			DELETE FROM dbo.exismovtodeta 
//				WHERE	mden_tipdoc = 1 
//				AND	mden_numero = :ll_numero
//				AND	bode_codigo = :li_bodzonal
//				USING sqlexi;
//			
//			IF sqlexi.SQLCode = -1 THEN
//				F_ErrorBaseDatos(sqlexi,"exismovtodeta")
//				Message.DoubleParm = -1
//				sqlexi.AutoCommit	=	ib_AutoCommit
//				RETURN 
//			END IF
//			
//			IF dw_exismovtodetanulos.Rowcount() > 0 THEN
//				lb_AutoCommit		=	sqlexi.AutoCommit
//				sqlexi.AutoCommit	=	False
//				
//				IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
//					
//					dw_exismovtodetanulos.ResetUpdate()
//		
//				ELSE
//					F_ErrorBaseDatos(sqlexi, This.Title)
//						
//					RollBack;
//					Return
//				END IF
//				
//				sqlexi.AutoCommit	=	lb_AutoCommit
//			
//			END IF
end if
		NEXT	
	NEXT
	//DISCONNECT USING sqlexi;
	//ib_Conectadoexistencia = False
END IF






























//Long 		ll_numero, respuesta, ll_fila, ll_nueva
//Boolean 	lb_AutoCommit
//Integer	li_bodecomercial
//
//IF ib_Conectadoexistencia = TRUE THEN
//	
//	SELECT DISTINCT expa_pacfri, expa_bodega, expa_pakcom
//	INTO :gi_bodvirtual, :gi_bodzonal, :li_bodecomercial
//	FROM dbo.existeparam
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"existeparam")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		Return
//	END IF
//	
//	SELECT max(mden_numero)
//	INTO :ll_numero
//	FROM dbo.exismovtoenca
//	WHERE mden_tipdoc = 1
//	AND bode_codigo = :gi_bodvirtual
//	AND mden_bodest = :gi_bodzonal
//	AND mden_docrel = :il_numeroenva
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		Return
//	END IF
//	
//	IF isnull(ll_numero) THEN
//		Return
//	END IF	
//	
//	UPDATE dbo.exismovtoenca SET
//	mden_estado = 2
//	WHERE mden_tipdoc = 1
//	AND bode_codigo = :gi_bodvirtual
//	AND mden_bodest = :gi_bodzonal
//	AND mden_numero = :ll_numero
//	AND mden_docrel = :il_numeroenva
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		RETURN 
//	END IF
//	
//	dw_exidetaborra.Retrieve(1,ll_numero)
//	
//	FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
//		ll_nueva = dw_exismovtodetanulos.InsertRow(0)
//		
//		dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
//		dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
//		dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
//		dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
//		dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
//		dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
//		//dw_exismovtodetanulos.Object.mdde_gratri[ll_nueva] = dw_exidetaborra.Object.mdde_codbor[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco1[ll_nueva] = dw_exidetaborra.Object.mdde_codbo1[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco2[ll_nueva] = dw_exidetaborra.Object.mdde_codbo2[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco3[ll_nueva] = dw_exidetaborra.Object.mdde_codbo3[ll_fila]  
//	NEXT	
//	
//	DELETE FROM dbo.exismovtodeta 
//	WHERE	mden_tipdoc = 1 
//	AND	mden_numero = :ll_numero
//	AND	bode_codigo = :gi_bodvirtual
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtodeta")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		RETURN 
//	END IF
//	
//	IF dw_exismovtodetanulos.Rowcount() > 0 THEN
//		lb_AutoCommit		=	sqlexi.AutoCommit
//		sqlexi.AutoCommit	=	False
//		
//		IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
//			
//			dw_exismovtodetanulos.ResetUpdate()
//
//		ELSE
//			F_ErrorBaseDatos(sqlexi, This.Title)
//				
//			RollBack;
//			Return
//		END IF
//		
//		sqlexi.AutoCommit	=	lb_AutoCommit
//	
//	END IF
//	
//	dw_exidetaborra.Reset()
//	dw_exismovtodetanulos.Reset()
//	DISCONNECT USING sqlexi;
//	
//END IF
//
//
end event

event anulamovto2();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
Date	ld_fecha		

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
	//	sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual 	= luo_existencia.bodvirtual
		li_bodzonal			= luo_existencia.bodzonal
		li_bodecomercial 	= luo_existencia.bodcomercial
	END IF	
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
	
	FOR ll_filote = 1 TO dw_3.RowCount()
		
		li_lote			= dw_3.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_3.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
		
//			IF li_categoria = 101 THEN
//				li_bodevirtual 	= luo_existencia.bodcomercial				
//			END IF
				
			il_numeroenva = dw_2.Object.mfge_numero[1]
			ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
			IF NOT luo_existencia.maximo_porfecha(3,il_packing,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			ELSE
				ll_numero = luo_existencia.numero
			END IF
					
			IF NOT isnull(ll_numero) THEN
				//Return
						
			IF NOT luo_existencia.actualizaexistencia(2,3,il_packing,ll_numero,il_numeroenva,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			END IF	
			
			dw_exidetaborra.Retrieve(3,ll_numero)
			
			FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
				ll_nueva = dw_exismovtodetanulos.InsertRow(0)
				
				dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
				dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
				dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
				dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
				dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
				dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
		
			NEXT
			
			DELETE FROM dbo.exismovtodeta 
				WHERE	mden_numero = :ll_numero
				AND	mden_tipdoc = 3
				AND	bode_codigo = :il_packing
				USING sqlexi;
			
			IF sqlexi.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlexi,"exismovtodeta")
				Message.DoubleParm = -1
				sqlexi.AutoCommit	=	ib_AutoCommit
				RETURN 
			END IF
			
			IF dw_exismovtodetanulos.Rowcount() > 0 THEN
				lb_AutoCommit		=	sqlexi.AutoCommit
				sqlexi.AutoCommit	=	False
				
				IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
					
					dw_exismovtodetanulos.ResetUpdate()
		
				ELSE
					F_ErrorBaseDatos(sqlexi, This.Title)
						
					RollBack;
					Return
				END IF
				
				sqlexi.AutoCommit	=	lb_AutoCommit
			
			END IF
		
//			IF NOT luo_existencia.nummaxpbodega(1,li_bodzonal,li_bodevirtual,il_numeroenva,True,sqlexi) THEN
//				F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//				Message.DoubleParm = -1
//				Return
//			ELSE	
//				ll_numero = luo_existencia.numero
//			END IF	
//		
//			IF isnull(ll_numero) THEN
//				Return
//			END IF	
//				
//			UPDATE dbo.exismovtoenca SET
//				mden_estado = 2
//				WHERE mden_tipdoc = 1
//				AND bode_codigo = :li_bodzonal
//				AND mden_numero = :ll_numero
//				AND mden_docrel = :il_numeroenva
//				AND mden_estado = 1
//				USING sqlexi;
//				
//			IF sqlexi.SQLCode = -1 THEN
//				F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//				Message.DoubleParm = -1
//				sqlexi.AutoCommit	=	ib_AutoCommit
//				Return
//			END IF	
//			
//			dw_exidetaborra.Retrieve(1,ll_numero)
//			
//			FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
//				ll_nueva = dw_exismovtodetanulos.InsertRow(0)
//				
//				dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
//				dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
//				dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
//				dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
//				dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
//				dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
//				dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
//			
//			NEXT
//			
//			DELETE FROM dbo.exismovtodeta 
//				WHERE	mden_tipdoc = 1 
//				AND	mden_numero = :ll_numero
//				AND	bode_codigo = :li_bodzonal
//				USING sqlexi;
//			
//			IF sqlexi.SQLCode = -1 THEN
//				F_ErrorBaseDatos(sqlexi,"exismovtodeta")
//				Message.DoubleParm = -1
//				sqlexi.AutoCommit	=	ib_AutoCommit
//				RETURN 
//			END IF
//			
//			IF dw_exismovtodetanulos.Rowcount() > 0 THEN
//				lb_AutoCommit		=	sqlexi.AutoCommit
//				sqlexi.AutoCommit	=	False
//				
//				IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
//					
//					dw_exismovtodetanulos.ResetUpdate()
//		
//				ELSE
//					F_ErrorBaseDatos(sqlexi, This.Title)
//						
//					RollBack;
//					Return
//				END IF
//				
//				sqlexi.AutoCommit	=	lb_AutoCommit
//			
//			END IF
end if
		NEXT	
	NEXT
	//DISCONNECT USING sqlexi;
	//ib_Conectadoexistencia = False
END IF






























//Long 		ll_numero, respuesta, ll_fila, ll_nueva
//Boolean 	lb_AutoCommit
//Integer	li_bodecomercial
//
//IF ib_Conectadoexistencia = TRUE THEN
//	
//	SELECT DISTINCT expa_pacfri, expa_bodega, expa_pakcom
//	INTO :gi_bodvirtual, :gi_bodzonal, :li_bodecomercial
//	FROM dbo.existeparam
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"existeparam")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		Return
//	END IF
//	
//	SELECT max(mden_numero)
//	INTO :ll_numero
//	FROM dbo.exismovtoenca
//	WHERE mden_tipdoc = 1
//	AND bode_codigo = :gi_bodvirtual
//	AND mden_bodest = :gi_bodzonal
//	AND mden_docrel = :il_numeroenva
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		Return
//	END IF
//	
//	IF isnull(ll_numero) THEN
//		Return
//	END IF	
//	
//	UPDATE dbo.exismovtoenca SET
//	mden_estado = 2
//	WHERE mden_tipdoc = 1
//	AND bode_codigo = :gi_bodvirtual
//	AND mden_bodest = :gi_bodzonal
//	AND mden_numero = :ll_numero
//	AND mden_docrel = :il_numeroenva
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		RETURN 
//	END IF
//	
//	dw_exidetaborra.Retrieve(1,ll_numero)
//	
//	FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
//		ll_nueva = dw_exismovtodetanulos.InsertRow(0)
//		
//		dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
//		dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
//		dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
//		dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
//		dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
//		dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
//		//dw_exismovtodetanulos.Object.mdde_gratri[ll_nueva] = dw_exidetaborra.Object.mdde_codbor[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco1[ll_nueva] = dw_exidetaborra.Object.mdde_codbo1[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco2[ll_nueva] = dw_exidetaborra.Object.mdde_codbo2[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco3[ll_nueva] = dw_exidetaborra.Object.mdde_codbo3[ll_fila]  
//	NEXT	
//	
//	DELETE FROM dbo.exismovtodeta 
//	WHERE	mden_tipdoc = 1 
//	AND	mden_numero = :ll_numero
//	AND	bode_codigo = :gi_bodvirtual
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtodeta")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		RETURN 
//	END IF
//	
//	IF dw_exismovtodetanulos.Rowcount() > 0 THEN
//		lb_AutoCommit		=	sqlexi.AutoCommit
//		sqlexi.AutoCommit	=	False
//		
//		IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
//			
//			dw_exismovtodetanulos.ResetUpdate()
//
//		ELSE
//			F_ErrorBaseDatos(sqlexi, This.Title)
//				
//			RollBack;
//			Return
//		END IF
//		
//		sqlexi.AutoCommit	=	lb_AutoCommit
//	
//	END IF
//	
//	dw_exidetaborra.Reset()
//	dw_exismovtodetanulos.Reset()
//	DISCONNECT USING sqlexi;
//	
//END IF
//
//
end event

public subroutine determina_productoresenvase (integer ai_tipomovto);str_Productores_Envases	lstr_Prod_Enva
DataWindow	ldw_envase
Long			ll_Fila, ll_GuiaSII, ll_Fila_Lote
Integer		li_Secuencia, li_Cliente, li_ClieAnt
Long			ll_Productor,ll_ProdAnt

//Inicializa la Estructura de Instancia
wstr_Prod_Enva	= lstr_Prod_Enva

IF ai_TipoMovto = 41 THEN
	ldw_envase	=	dw_5
ELSE
	ldw_envase	=	dw_7
END IF

FOR ll_Fila	=	1 TO ldw_Envase.RowCount()
	ll_Productor	=	ldw_Envase.Object.prod_codigo[ll_Fila]
   li_Cliente		=	ldw_Envase.Object.clie_codigo[ll_Fila]
	
	IF (ll_Productor <> ll_ProdAnt) AND (li_Cliente <> li_ClieAnt) THEN
		ll_ProdAnt	=	ll_Productor
		li_ClieAnt	=	li_Cliente
		li_Secuencia ++
		
		IF ai_TipoMovto = 41 THEN
			ll_Fila_Lote	=	dw_3.Find(" prod_codigo = "+String(ll_Productor),1,dw_3.RowCount())
			
			IF ll_Fila_Lote > 0 THEN
				ll_GuiaSII	=	dw_3.Object.lote_guisii[ll_Fila_Lote]
			ELSE
				ll_GuiaSII	=	0
			END IF
		END IF
		
		wstr_Prod_Enva.Productor[li_Secuencia]	=	ll_Productor
		wstr_Prod_Enva.GuiaSII[li_Secuencia]	=	ll_GuiaSII
		wstr_Prod_Enva.TipoMovto[li_Secuencia]	=	ai_TipoMovto
		wstr_Prod_Enva.Cliente[li_Secuencia]	=	li_Cliente
	END IF
NEXT

RETURN
end subroutine

protected function integer wf_modifica ();IF dw_3.AcceptText() = -1 THEN RETURN -1
IF dw_5.AcceptText() = -1 THEN RETURN -1
IF dw_7.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF (dw_3.ModifiedCount() + dw_3.DeletedCount()) > 0 THEN RETURN 0
IF (dw_5.ModifiedCount() + dw_5.DeletedCount()) > 0 THEN RETURN 0
IF (dw_7.ModifiedCount() + dw_7.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public subroutine destare (boolean ab_actualiza);Long			ll_Fila, li_filas, ll_Fila_lote
Integer		li_Especie, li_TipoEnvase, li_Envase, li_TipoAnt, li_EnvaAnt,&
				li_TotBultos, li_Secuencia, li_Lote, li_LoteAnt, li_BultosLote, li_Cliente
Boolean		lb_FueraRango
Date			ld_Fechamovto
Decimal{3}	ld_TotalNeto, ld_PesoEstandar[], ld_PesoMinimo[], ld_PesoMaximo[],&
				ld_TotalEstandar, ld_PesoDistrib, ld_LoteDistrib, ld_NetoLoteEnvase,&
				ld_TotalDistrib, ld_Remanente
Double		ld_Factor
Decimal 		Ld_entradaa, Ld_entradab, Ld_salidaa, Ld_salidab, ld_enven, ld_envsan, ld_TaraBultos

Ld_entradaa						=	dw_2.Object.refg_tkbent[1] 
Ld_entradab						=	dw_2.Object.refg_tkbenc[1]
Ld_salidaa						= 	dw_2.Object.refg_tkbsal[1]
Ld_salidab						=	dw_2.Object.refg_tkbsac[1] 
ld_enven							=	dw_2.Object.refg_tkenen[1]
ld_envsan						=	dw_2.Object.refg_tkensa[1]
dw_2.Object.mfge_tpneto[1] = 	(Ld_entradaa - Ld_salidaa) + ( Ld_entradab - Ld_salidab) - ( ld_enven + ld_envsan)

li_Especie						=	dw_2.Object.espe_codigo[1]
ld_FechaMovto					=	dw_2.Object.mfge_fecmov[1]
ld_TotalNeto					=	dw_2.Object.mfge_tpneto[1]
li_Cliente						=	dw_2.Object.clie_codigo[1]

dw_3.SetSort("lote_pltcod A, lote_espcod A, lote_codigo A")
dw_3.Sort()

dw_6.SetSort("lote_pltcod A, lote_espcod A, lote_codigo A, enva_tipoen A, enva_codigo A")
dw_6.Sort()

FOR ll_Fila = 1 TO dw_6.RowCount()
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]
	
	IF li_TipoEnvase <> li_TipoAnt OR li_Envase <> li_EnvaAnt THEN
		IF iuo_PesoEstanEspe.Existe(li_Especie, li_TipoEnvase, li_Envase, ld_FechaMovto, True, SQLCA) THEN
			li_Secuencia ++
			ld_PesoEstandar[li_Secuencia]	=	iuo_PesoEstanEspe.PesoDistrib
			ld_PesoMinimo[li_Secuencia]	=	iuo_PesoEstanEspe.PesoMinimo
			ld_PesoMaximo[li_Secuencia]	=	iuo_PesoEstanEspe.PesoMaximo
			
		ELSE
			RETURN
		END IF
		li_TipoAnt		=	li_TipoEnvase
		li_EnvaAnt		=	li_Envase
	END IF
	
	li_TotBultos		=	dw_6.Object.lotd_totbul[ll_Fila]
	ld_TotalEstandar	=	ld_TotalEstandar + (li_TotBultos*ld_PesoEstandar[li_Secuencia])
	
NEXT

IF IsNull(ld_TotalEstandar) OR ld_TotalEstandar = 0 THEN
	Return
END IF

ld_Factor		=	ld_TotalNeto / ld_TotalEstandar
li_Secuencia	=	0
li_TipoAnt		=	0
li_EnvaAnt		=	0

FOR ll_Fila = 1 TO dw_6.RowCount()
	li_Lote			=	dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]
	
	IF li_Lote <> li_LoteAnt THEN
		IF li_LoteAnt > 0 THEN
			ld_PesoDistrib = Round(ld_LoteDistrib / li_BultosLote,ii_kildec)
			IF ab_actualiza THEN
				dw_3.Object.lote_kilpro[ll_Fila_lote]	=	ld_PesoDistrib
				dw_3.Object.lote_totnet[ll_Fila_lote]	=	ld_LoteDistrib
				
			END IF
			
			IF lb_FueraRango THEN
				dw_3.Object.font[ll_Fila_lote]	=	1
				
			ELSE
				dw_3.Object.font[ll_Fila_lote]	=	0
				
			END IF
		END IF
		
		ll_Fila_lote ++
		li_BultosLote	=	dw_3.Object.lote_totbul[ll_Fila_lote]
		ld_LoteDistrib	=	0
		li_LoteAnt		=	li_Lote
		lb_FueraRango	=	False
		
	END IF
	
	IF li_TipoEnvase <> li_TipoAnt OR li_Envase <> li_EnvaAnt THEN
		li_Secuencia ++
		li_TipoAnt	=	li_TipoEnvase
		li_EnvaAnt	=	li_Envase
		
	END IF
	
	li_TotBultos		=	dw_6.Object.lotd_totbul[ll_Fila]
	ld_PesoDistrib		=	Round(ld_PesoEstandar[li_Secuencia] * ld_Factor,ii_kildec)
	ld_NetoLoteEnvase	=	Round(ld_PesoDistrib * li_TotBultos,ii_kildec)
	
	IF ld_PesoDistrib < ld_PesoMinimo[li_Secuencia] OR ld_PesoDistrib > ld_PesoMaximo[li_Secuencia] THEN
		dw_6.Object.font[ll_Fila]	=	1
		lb_FueraRango					=	True
		
	ELSE
		dw_6.Object.font[ll_Fila]	=	0
		
	END IF
	
	IF ab_actualiza THEN
		dw_6.Object.lotd_kilpro[ll_Fila]	=	ld_PesoDistrib
		dw_6.Object.lotd_totnet[ll_Fila]	=	ld_NetoLoteEnvase
		
	END IF
	
	ld_LoteDistrib		=	ld_LoteDistrib 	+ ld_NetoLoteEnvase
	ld_TotalDistrib	=	ld_TotalDistrib 	+ ld_NetoLoteEnvase
	
NEXT

IF li_LoteAnt > 0 THEN
	ld_PesoDistrib = Round(ld_LoteDistrib / li_BultosLote,ii_kildec)
	
	IF ab_actualiza THEN
		dw_3.Object.lote_kilpro[ll_Fila_lote]	=	ld_PesoDistrib
		dw_3.Object.lote_totnet[ll_Fila_lote]	=	ld_LoteDistrib
		
	END IF
	
	IF lb_FueraRango THEN
		dw_3.Object.font[ll_Fila_lote]	=	1
		
	ELSE
		dw_3.Object.font[ll_Fila_lote]	=	0
		
	END IF
END IF

IF ab_actualiza THEN
	ld_Remanente = ld_TotalDistrib - ld_TotalNeto
	
	IF ld_Remanente <> 0 THEN
		ld_NetoLoteEnvase									=	dw_6.Object.lotd_totnet[dw_6.RowCount()]
		dw_6.Object.lotd_totnet[dw_6.RowCount()]	=	ld_NetoLoteEnvase - ld_Remanente
		ld_NetoLoteEnvase									=	dw_3.Object.lote_totnet[dw_3.RowCount()]
		dw_3.Object.lote_totnet[dw_3.RowCount()]	=	ld_NetoLoteEnvase - ld_Remanente
	END IF
	
	ib_Destare			=	True
	pb_grabar.Enabled	=	True

END IF
end subroutine

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_hora
Integer	li_Estado

li_estado	=	dw_2.Object.mfge_estmov[1]

IF Isnull(li_estado) THEN li_estado = 1

IF as_Columna <> "mfge_fecmov" AND &
	(dw_2.Object.mfge_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfge_fecmov[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "defg_docrel" and gb_RecepcionDeProceso AND &
	(dw_2.Object.defg_docrel[1] = 0 OR IsNull(dw_2.Object.defg_docrel[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "refg_horaen" AND &
	(dw_2.Object.refg_horaen[1] = lt_hora OR IsNull(dw_2.Object.refg_horaen[1])) THEN
	lb_Estado = False
END IF

IF NOT gb_RecepcionDeProceso THEN
	
	IF as_Columna <> "tran_codigo" AND &
		(dw_2.Object.tran_codigo[1] = 0 OR IsNull(dw_2.Object.tran_codigo[1])) THEN
		lb_Estado = False
	END IF
		
	IF as_Columna <> "cami_patent" AND &
		(dw_2.Object.cami_patent[1] = "" OR IsNull(dw_2.Object.cami_patent[1])) THEN
		lb_Estado = False
	END IF
		
	IF as_Columna <> "mfge_chofer" AND &
		(dw_2.Object.mfge_chofer[1] = "" OR IsNull(dw_2.Object.mfge_chofer[1])) THEN
		lb_Estado = False
	END IF
	
END IF

//IF as_Columna <> "refg_tkbent" AND &
//	(dw_2.Object.refg_tkbent[1] = 0 OR IsNull(dw_2.Object.refg_tkbent[1])) THEN
//	lb_Estado = False
//END IF

IF as_Columna <> "mfge_totbul" AND &
	(dw_2.Object.mfge_totbul[1] = 0 OR IsNull(dw_2.Object.mfge_totbul[1])) THEN
	lb_Estado = False
END IF

IF li_Estado = 3 THEN 
	lb_Estado	=	False
END IF

tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.mfge_numero.Protect			=	0
	dw_2.Object.espe_codigo.Protect				=	0
	dw_2.Object.tran_codigo.Protect				=	0
	dw_2.Object.cami_patent.Protect				=	0
	dw_2.Object.cami_patcar.Protect				=	0
	dw_2.Object.mfge_rutcho.Protect				=	0
	dw_2.Object.mfge_chofer.Protect				=	0
	dw_2.Object.mfge_totbul.Protect				=	0
	dw_2.Object.refg_tkbent.Protect				=	0
	dw_2.Object.refg_tkbenc.Protect				=	0
	dw_2.Object.mfge_fecmov.Protect				=	0
	dw_2.Object.refg_horaen.Protect				=	0
	dw_2.Object.mfge_totbul.Protect				=	0
	dw_2.Object.mfge_observ.Protect				=	0
	
	dw_2.Object.mfge_numero.Color	=	0
	dw_2.Object.espe_codigo.Color	=	0
	dw_2.Object.tran_codigo.Color		=	0	
	dw_2.Object.cami_patent.Color	=	0
	dw_2.Object.cami_patcar.Color	=	0
	dw_2.Object.mfge_rutcho.Color	=	0
	dw_2.Object.mfge_chofer.Color	=	0
	dw_2.Object.mfge_totbul.Color		=	0
	dw_2.Object.refg_tkbent.Color		=	0
	dw_2.Object.refg_tkbenc.Color		=	0
	dw_2.Object.mfge_fecmov.Color	=	0
	dw_2.Object.refg_horaen.Color	=	0
	dw_2.Object.mfge_totbul.Color		=	0
	dw_2.Object.mfge_observ.Color	=	0
	
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_totbul.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.refg_tkbent.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.refg_tkbenc.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.mfge_fecmov.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_horaen.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.mfge_totbul.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.mfge_observ.BackGround.Color	=	RGB(255,255,255)	

	dw_2.Object.refg_tkbsal.Protect				=	1
	dw_2.Object.refg_tkbsac.Protect				=	1
	dw_2.Object.refg_horasa.Protect				=	1
	dw_2.Object.refg_fecsal.Protect				=	1
	
	dw_2.Object.refg_tkbsal.Color	=	RGB(255,255,255)
	dw_2.Object.refg_tkbsac.Color	=	RGB(255,255,255)
	dw_2.Object.refg_horasa.Color	=	RGB(255,255,255)
	dw_2.Object.refg_fecsal.Color	=	RGB(255,255,255)
	
	dw_2.Object.refg_tkbsal.BackGround.Color	=	553648127
	dw_2.Object.refg_tkbsac.BackGround.Color	=	553648127
	dw_2.Object.refg_horasa.BackGround.Color	=	553648127
	dw_2.Object.refg_fecsal.BackGround.Color	=	553648127
	
	Tab_1.Tp_3.Enabled								=	False
Else
	dw_2.Object.mfge_numero.Protect			=	1
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.tran_codigo.Protect				=	1
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.mfge_rutcho.Protect				=	1
	dw_2.Object.mfge_chofer.Protect				=	1
	
	dw_2.Object.mfge_numero.Color	=	RGB(255,255,255)
	dw_2.Object.espe_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.tran_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.Color	=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.Color	=	RGB(255,255,255)
	
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color	=	553648127
	
	If dw_2.Object.mfge_estmov[1] = 3 Then
		dw_2.Object.refg_tkbent.Protect				=	1
		dw_2.Object.refg_tkbenc.Protect				=	1
		dw_2.Object.mfge_fecmov.Protect				=	1
		dw_2.Object.refg_horaen.Protect				=	1
		dw_2.Object.mfge_totbul.Protect				=	1
		dw_2.Object.mfge_observ.Protect				=	1
		
		dw_2.Object.refg_tkbent.Color		=	0
		dw_2.Object.refg_tkbenc.Color		=	0
		dw_2.Object.mfge_fecmov.Color	=	0
		dw_2.Object.refg_horaen.Color	=	0
		dw_2.Object.mfge_totbul.Color		=	0	
		dw_2.Object.mfge_observ.Color	=	0
		
		dw_2.Object.refg_tkbent.BackGround.Color		=	553648127	
		dw_2.Object.refg_tkbenc.BackGround.Color		=	553648127	
		dw_2.Object.mfge_fecmov.BackGround.Color	=	553648127	
		dw_2.Object.refg_horaen.BackGround.Color		=	553648127	
		dw_2.Object.mfge_totbul.BackGround.Color		=	553648127	
		dw_2.Object.mfge_observ.BackGround.Color	=	553648127	
	End If
End If

dw_2.Object.buscacamion.Enabled	=	NOT gb_RecepcionDeProceso

If gb_RecepcionDeProceso Then
	dw_2.Object.tran_codigo.Protect					=	1
	dw_2.Object.cami_patent.Protect					=	1
	dw_2.Object.cami_patcar.Protect					=	1
	dw_2.Object.mfge_rutcho.Protect					=	1
	dw_2.Object.mfge_chofer.Protect					=	1

	dw_2.Object.tran_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.Color	=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.Color	=	RGB(255,255,255)
	
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color	=	553648127
Else
	dw_2.Object.tran_codigo.Protect					=	0
	dw_2.Object.cami_patent.Protect					=	0
	dw_2.Object.cami_patcar.Protect					=	0
	dw_2.Object.mfge_rutcho.Protect					=	0
	dw_2.Object.mfge_chofer.Protect					=	0

	dw_2.Object.tran_codigo.Color		=	0
	dw_2.Object.cami_patent.Color	=	0
	dw_2.Object.cami_patcar.Color	=	0
	dw_2.Object.mfge_rutcho.Color	=	0
	dw_2.Object.mfge_chofer.Color	=	0
	
	dw_2.Object.tran_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patent.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.cami_patcar.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_rutcho.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfge_chofer.BackGround.Color	=	RGB(255,255,255)
End If
end subroutine

public subroutine habilitagrabacion (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora

IF as_Columna <> "refg_fecsal" AND &
	(dw_2.Object.refg_fecsal[1] = ld_Fecha OR IsNull(dw_2.Object.refg_fecsal[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "refg_horasa" AND &
	(dw_2.Object.refg_horasa[1] = lt_Hora OR IsNull(dw_2.Object.refg_horasa[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "refg_tkbsal" AND &
	(dw_2.Object.refg_tkbent[1] = 0 OR IsNull(dw_2.Object.refg_tkbent[1])) THEN
	lb_Estado = False
END IF

pb_Grabar.Enabled	=	lb_Estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfge_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horacr",F_FechaHora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"mfge_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horact",F_FechaHora())
		
		IF dw_3.DeletedCount() > 0 THEN Borrando = True
		IF dw_6.DeletedCount() > 0 THEN Borrando = True
	END IF
END IF

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN 								//Detalle
		IF dw_6.Update(True, False) = 1 THEN							//Detalle de Lotes
			IF dw_3.Update(True, False) = 1 THEN						//Lotes
				IF dw_9.Update(True, False) = 1 THEN					//Detalle de Pesaje
					IF dw_2.Update(True, False) = 1 THEN				//Encabezado
						IF dw_5.Update(True,False) = 1 THEN			//Envases Recepcionados
							IF dw_7.Update(True,False) = 1 THEN		//Envases Retirados
								IF dw_4.Update(True,False) = 1 THEN	//Encabezados Envases
									IF dw_spro_bins.Update(True,False) = 1 THEN
										Commit;
									
										IF sqlca.SQLCode <> 0 THEN
											F_ErrorBaseDatos(sqlca, This.Title)
											RollBack;
										ELSE
											lb_Retorno	=	True
										
											dw_9.ResetUpdate()
											dw_6.ResetUpdate()
											dw_7.ResetUpdate()
											dw_5.ResetUpdate()
											dw_4.ResetUpdate()
											dw_3.ResetUpdate()
											dw_2.ResetUpdate()
											dw_1.ResetUpdate()
											dw_spro_bins.ResetUpdate()
										END IF
									ELSE
										F_ErrorBaseDatos(sqlca, This.Title)
										RollBack;
									END IF
								ELSE
									F_ErrorBaseDatos(sqlca, This.Title)
									RollBack;
								END IF
							ELSE
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							END IF
						ELSE
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						END IF
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN		 						//Encabezado
		IF dw_3.Update(True, False) = 1 THEN							//Lotes
			IF dw_6.Update(True, False) = 1 THEN						//Detalle de Lotes
				IF dw_1.Update(True, False) = 1 THEN					//Detalle
					IF dw_4.Update(True,False) = 1 THEN				//Encabezado Envases
						IF dw_5.Update(True,False) = 1 THEN			//Envases Recepcionados
							IF dw_7.Update(True,False) = 1 THEN		//Envases Retirados
								IF dw_9.Update(True,False) = 1 THEN	//Detalle Pesaje
									IF dw_spro_bins.Update(True,False) = 1 THEN
										IF Actual_ultimo_lote() THEN
	
											IF ib_destare and gstr_paramplanta.packing THEN
												IF PROCESOPACKING() THEN
													MessageBox("Atención", "Se ha terminado el proceso Automático en forma normal.")
													Commit;
												ELSE
													MessageBox("Atención", "El proceso automático ha generado errores, No se ha podido grabar los datos.")
													ROLLBACK;
												END IF
											ELSE
												Commit;
											END IF
										
											IF sqlca.SQLCode <> 0 THEN
												F_ErrorBaseDatos(sqlca, This.Title)
											ELSE
												lb_Retorno	=	True
												
												dw_9.ResetUpdate()
												dw_6.ResetUpdate()
												dw_7.ResetUpdate()
												dw_5.ResetUpdate()
												dw_4.ResetUpdate()
												dw_3.ResetUpdate()
												dw_2.ResetUpdate()
												dw_1.ResetUpdate()
												dw_spro_bins.ResetUpdate()
											END IF
										ELSE
											F_ErrorBaseDatos(sqlca, This.Title)
											RollBack;
										END IF
									ELSE
										F_ErrorBaseDatos(sqlca, This.Title)
										RollBack;
									END IF
								ELSE
									F_ErrorBaseDatos(sqlca, This.Title)
									RollBack;
								END IF
							ELSE
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							END IF
						ELSE
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						END IF
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public function boolean chequea_envasesproductor ();Boolean	lb_Retorno	=	True
//Long		ll_Fila,  ll_Fila_det,ll_Productor 
//Integer	li_TipoEnvase, li_Envase, li_Bultos, li_BultosAnt, li_posicion,&
//			li_Bultos_Enc, li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo, li_Total_Bultos,&
//			li_Cliente
//String	ls_linea_chequeo, ls_busqueda
//
//li_Bultos_Enc	=	dw_2.Object.mfge_totbul[1]
//li_Cliente		=	dw_2.Object.clie_codigo[1]
//
//FOR ll_Fila = 1 TO dw_3.RowCount()
//	
//	li_Lote_pltcod	=	dw_3.Object.lote_pltcod[ll_Fila]
//	li_Lote_espcod	=	dw_3.Object.lote_espcod[ll_Fila]
//	li_Lote_codigo	=	dw_3.Object.lote_codigo[ll_Fila]
//	ll_Productor	=	dw_3.Object.prod_codigo[ll_Fila]
//	
//	dw_6.SetFilter("lote_pltcod = "+String(li_Lote_pltcod)+" and "+&
//					   "lote_espcod = "+String(li_Lote_espcod)+" and "+&
//						"lote_codigo = "+String(li_Lote_codigo))
//	dw_6.Filter()
//	
//	FOR ll_Fila_det = 1 TO dw_6.RowCount()
//		li_TipoEnvase		=	dw_6.Object.enva_tipoen[ll_Fila_det]
//		li_Envase			=	dw_6.Object.enva_codigo[ll_Fila_det]
//		li_Bultos			=	dw_6.Object.lotd_totbul[ll_Fila_det]
//		li_Total_Bultos	=	li_Total_Bultos + li_Bultos
//		
//		ls_busqueda		= 'P'+String(ll_Productor,'0000')+'T'+String(li_TipoEnvase)+'E'+String(li_Envase,'000')
//		li_posicion 	= 	Pos(ls_linea_chequeo,ls_busqueda)
//		
//		IF li_posicion = 0 THEN
//			ls_linea_chequeo	=	ls_linea_chequeo + ls_busqueda + 'B' + String(li_Bultos,'0000') + 'I0000'
//		ELSE
//			li_BultosAnt		=	Integer(Mid(ls_linea_chequeo,li_posicion + 12, 4))
//			li_Bultos			=	li_BultosAnt + li_Bultos
//			ls_linea_chequeo	=	Mid(ls_linea_chequeo, 1, li_posicion - 1) + &
//										ls_busqueda + 'B' + String(li_Bultos,'0000') + 'I0000' + &
//										Mid(ls_linea_chequeo, li_posicion + 21)
//		END IF
//	NEXT
//	dw_6.SetFilter("")
//	dw_6.Filter()
//NEXT
//
//FOR ll_Fila = 1 TO dw_5.RowCount()
//	ll_Productor	=	dw_5.Object.prod_codigo[ll_Fila]
//	li_TipoEnvase	=	dw_5.Object.enva_tipoen[ll_Fila]
//	li_Envase		=	dw_5.Object.enva_codigo[ll_Fila]
//	li_Bultos		=	dw_5.Object.fgme_cantid[ll_Fila]
//	
//	IF dw_5.Object.fgme_conenv[ll_Fila] = 1 THEN
//		ls_busqueda		= 'P'+String(ll_Productor,'00000')+'T'+String(li_TipoEnvase)+'E'+String(li_Envase,'000')
//		li_posicion 	= 	Pos(ls_linea_chequeo,ls_busqueda)
//		
//		IF li_posicion = 0 THEN
//			ls_linea_chequeo	=	ls_linea_chequeo + ls_busqueda + 'B0000' + 'I' + String(li_Bultos,'0000') 
//		ELSE
//			li_BultosAnt		=	Integer(Mid(ls_linea_chequeo,li_posicion + 17, 4))
//			li_Bultos			=	li_BultosAnt + li_Bultos
//			ls_linea_chequeo	=	Mid(ls_linea_chequeo, 1, li_posicion + 16 ) + &
//										String(li_Bultos,'0000') + Mid(ls_linea_chequeo, li_posicion + 21)
//		END IF
//	END IF
//NEXT
//
//IF lb_Retorno THEN
//	IF li_Total_Bultos <> li_Bultos_Enc THEN
//		MessageBox("Atención","Total Envases con Fruta no corresponde a Total Bultos recepcionados")
//		lb_Retorno	=	False
//	END IF
//END IF

RETURN lb_Retorno
end function

public subroutine productoreslotes (ref string productores[]);Long		ll_Fila
Integer	li_Secuencia
String	ls_productor, ls_ProdAnt, ls_Nula[]
productoresxlote	istr_pxl

Productores	=	ls_Nula

FOR ll_Fila = 1 TO dw_3.RowCount()
	ls_Productor	=	dw_3.Object.prod_rut[ll_Fila]
	
	istr_pxl.prod_rut[ll_fila]			=	dw_3.Object.prod_rut[ll_Fila]
	istr_pxl.prod_codigo[ll_fila]		=	dw_3.Object.prod_codigo[ll_Fila]
	istr_pxl.prod_nombre[ll_fila]	=	dw_3.Object.prod_nombre[ll_Fila]
	istr_pxl.lote_codigo[ll_fila]		=	dw_3.Object.lote_codigo[ll_Fila]
	istr_pxl.lote_pltcod[ll_fila]		=	dw_3.Object.lote_pltcod[ll_Fila]
	istr_pxl.lote_espcod[ll_fila]		=	dw_3.Object.lote_espcod[ll_Fila]

	IF ls_Productor <> ls_ProdAnt THEN
		li_Secuencia ++
		Productores[li_Secuencia]	=	ls_Productor
	
		ls_ProdAnt	=	ls_Productor
	END IF
NEXT

//Integer li_filas_dw6, li_filas_dw4, li_bulto
//
//dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
//
//FOR li_filas_dw6 = 1 TO dw_6.RowCount()
//	
//	li_filas_dw4	=	dw_4.find( 'enva_tipoen = ' + String(dw_6.Object.enva_tipoen[li_filas_dw6]) + ' and ' +&
//									   'enva_codigo = ' + String(dw_6.Object.enva_codigo[li_filas_dw6]) + ' and ' +&
//										 + ' and ' +&, 1, dw_2.RowCount() )
//										
//	IF li_filas_dw4 < 1 THEN
//		li_filas_dw4	=	dw_4.InsertRow(0)
//		dw_4.Object.lote_pltcod[li_filas_dw4]	=	Integer(istr_mant.Argumento[1])
//		dw_4.Object.lote_espcod[li_filas_dw4]	=	Integer(istr_mant.Argumento[5])
//		dw_4.Object.lote_codigo[li_filas_dw4]	=	dw_6.Object.lote_codigo[li_filas_dw6]
//		dw_4.Object.enva_tipoen[li_filas_dw4]	=	dw_6.Object.enva_tipoen[li_filas_dw6]
//		dw_4.Object.enva_codigo[li_filas_dw4]	=	dw_6.Object.enva_codigo[li_filas_dw6]
//	END IF
//	
//	li_bulto 											=	dw_4.Object.lotd_totbul[li_filas_dw4]
//	
//	IF IsNull(li_bulto) THEN li_bulto = 0
//	
//	dw_4.Object.lotd_totbul[li_filas_dw4] 	=	li_bulto + 1
//	
//NEXT

RETURN
end subroutine

public function boolean noexistechofer (string rut);String ls_nombre,ls_paterno,ls_materno

SELECT 	clpr_nombre,clpr_apepat,clpr_apemat
INTO		:ls_nombre, :ls_paterno , :ls_materno
FROM 		dbo.clienprove
WHERE		clpr_nrorut =:rut;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClienProve")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Rut No ha sido Generado. Ingrese Otro.")
	Return True
END IF

is_chofer	= 	ls_nombre + " " + ls_paterno + " " + ls_materno
Return False





end function

public subroutine habilitasalida ();dw_2.Object.refg_tkbsal.Protect				=	0
dw_2.Object.refg_tkbsal.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.refg_tkbsac.Protect				=	0
dw_2.Object.refg_tkbsac.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.refg_horasa.Protect				=	0
dw_2.Object.refg_horasa.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.refg_fecsal.Protect					=	0
dw_2.Object.refg_fecsal.BackGround.Color		=	RGB(255,255,255)
tab_1.tp_3.Enabled					=	True

dw_2.Object.refg_tkbent.Protect					=	1
dw_2.Object.refg_tkbent.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.refg_tkbenc.Protect					=	1
dw_2.Object.refg_tkbenc.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.mfge_fecmov.Protect					=	1
dw_2.Object.mfge_fecmov.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.refg_horaen.Protect					=	1
dw_2.Object.refg_horaen.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.mfge_totbul.Protect					=	1
dw_2.Object.mfge_totbul.BackGround.Color		=	RGB(166,180,210)

ib_Salida						=	True

dw_2.Object.destare.Text	=	'Destare'

idt_FechaSistema				=	F_FechaHora()


dw_2.Object.refg_fecsal[1]	=	Date(String(idt_FechaSistema,'dd/mm/yyyy'))
dw_2.Object.refg_horasa[1]	=	Time(Mid(String(idt_FechaSistema),12,8))

pb_grabar.Enabled				=	False
pb_imprimir.Enabled			=	False

captura_totales()
end subroutine

public subroutine actualizakiloslote ();Long			ll_Fila, ll_Filas
Integer		li_Planta_Lote, li_Especie_Lote, li_Numero_Lote
Decimal{3}	ld_Kilos_Neto
Boolean		lb_Actualiza

uo_duchacontrol	luo_duchacontrol

luo_duchacontrol	=	Create uo_duchacontrol

ll_Filas	=	dw_3.RowCount()

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

FOR	ll_Fila	=	1 TO ll_Filas
	IF dw_3.Object.lote_ducha[ll_Fila]	=	1 THEN
		li_Planta_Lote		=	dw_3.Object.lote_pltcod[ll_Fila]
		li_Especie_Lote	=	dw_3.Object.lote_espcod[ll_Fila]
		li_Numero_Lote		=	dw_3.Object.lote_codigo[ll_Fila]
		ld_Kilos_Neto		=	dw_3.Object.lote_totnet[ll_Fila]
		
		IF luo_duchaControl.TieneDuchaLote(li_Planta_Lote, li_Especie_Lote, li_Numero_Lote, False, SQLCA) THEN
			IF luo_duchaControl.ActualizaKilosLote(li_Planta_Lote, li_Especie_Lote, li_Numero_Lote, ld_Kilos_Neto, SQLCA) THEN
				lb_actualiza	=	True
			END IF
		END IF
	END IF
NEXT

Destroy luo_duchacontrol

IF lb_Actualiza THEN
	Commit;
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN
end subroutine

public subroutine buscacamion ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = '1'

OpenWithParm(w_busc_camiones, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.cami_clasifi[il_fila]	=	Integer(lstr_busq.argum[1])
	dw_2.Object.cami_patent[il_fila]		=	lstr_busq.argum[2]
	dw_2.Object.cami_patcar[il_fila]		=	lstr_busq.argum[6]
	dw_2.Object.mfge_rutcho[il_fila]		=	lstr_busq.argum[5]
	is_rut = lstr_busq.argum[5]
	dw_2.Object.mfge_chofer[il_fila]		=	lstr_busq.argum[4]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public subroutine captura_totales ();Long			ll_Total_Bultos, ll_Fila
Decimal{3}	ld_Total_PesoEnv_Ent,ld_Total_PesoEnv_Sal, ld_Total_Neto,&
				ld_Total_KBE, ld_Total_KBS, ld_KBE_Camion, ld_KBE_Carro,&
				ld_KBS_Camion, ld_KBS_Carro, ld_TaraCamion, ld_TaraCarro

dw_2.AcceptText()
dw_3.AcceptText()
dw_5.AcceptText()
dw_7.AcceptText()

ll_Fila	=	dw_3.RowCount()
 
IF ll_Fila > 0 THEN
	dw_3.GroupCalc()
	IF dw_3.Object.total_bultos[ll_fila] > 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		RETURN
	ELSE
		ll_Total_Bultos		=	dw_3.GetItemNumber(ll_Fila,"total_bultos")
	END IF
END IF

ll_Fila	=	dw_5.RowCount()

IF ll_Fila > 0 THEN
	dw_5.GroupCalc()
	IF dw_5.Object.total_pesoenv[ll_Fila] >= 100000 THEN
		Messagebox("Error de Consistencia","El peso de los envase supera lo permitido")
		RETURN
	ELSE
		ld_Total_PesoEnv_Ent	=	dw_5.Object.total_pesoenv[ll_Fila]
	END IF
	
	IF dw_5.Object.total_envases[ll_Fila] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		RETURN
	END IF
END IF

ll_Fila	=	dw_7.RowCount()

IF ll_Fila > 0 THEN
	dw_7.GroupCalc()
	ld_Total_PesoEnv_Sal	=	dw_7.Object.total_pesoenv[ll_Fila]
END IF

//Determina Peso Neto del Camión
IF ib_Salida THEN
	ld_KBE_Camion	=	dw_2.Object.refg_tkbent[1]
	ld_KBE_Carro	=	dw_2.Object.refg_tkbenc[1]
	
	IF Isnull(ld_KBE_Carro) THEN ld_KBE_Carro = 0
	
	ld_Total_KBE	=	ld_KBE_Camion + ld_KBE_Carro
	
	ld_KBS_Camion	=	dw_2.Object.refg_tkbsal[1]
	ld_KBS_Carro	=	dw_2.Object.refg_tkbsac[1]
	ld_TaraCamion	=	iuo_Camion.TaraCamion
	ld_TaraCarro	=	iuo_Camion.TaraCarro
	
	IF Isnull(ld_TaraCamion) THEN ld_TaraCamion = 0
	IF Isnull(ld_TaraCarro) THEN ld_TaraCarro = 0
	
	IF ld_KBS_Camion + ld_KBS_Carro > 0 AND &
		ld_KBS_Camion + ld_KBS_Carro < ld_Total_PesoEnv_Sal THEN
		MessageBox("Atención","Kilos de salida no pueden ser menor a los envases de salida")
		RETURN
	ELSE
		ld_Total_KBS	=	ld_KBS_Camion + ld_KBS_Carro - ld_Total_PesoEnv_Sal
	END IF
		
//	ld_Total_Neto	=	ld_Total_KBE - ld_Total_PesoEnv_Ent - ld_Total_KBS
	ld_Total_Neto	=	ld_Total_KBE - ld_Total_PesoEnv_Ent

	dw_2.Object.refg_tkensa[1]	=	ld_Total_PesoEnv_Sal
	dw_2.Object.mfge_tpneto[1]	=	ld_Total_Neto

END IF

dw_2.Object.refg_tkenen[1]	=	ld_Total_PesoEnv_Ent

IF ib_Destare THEN
	Destare(True)
END IF

RETURN
end subroutine

public function boolean actual_ultimo_lote ();Long 		ll_Filas, ll_Fila, ll_Lote
Integer 	li_Lote_espcod, li_planta
String 	ls_nombrepc

li_Planta	=	Integer(istr_mant.Argumento[1])
ll_Filas		=	dw_3.RowCount()
ll_Fila		=	0
ll_Lote		=	0

DO WHILE ll_Fila < ll_Filas
		ll_Fila = dw_3.GetNextModified(ll_fila, Primary!)
		IF ll_Fila > 0 AND dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
			IF ll_Lote < dw_3.Object.lote_codigo[ll_Fila] THEN
				ll_lote 			=	dw_3.Object.lote_codigo[ll_Fila]
				li_Lote_espcod	=	dw_3.Object.lote_espcod[ll_Fila]
			END IF
		END IF
LOOP

IF ll_Lote  > 0 THEN
	ls_nombrepc	=	gstr_us.computador
	
	UPDATE	dbo.spro_lotescorrelequipo
		SET	loco_ultcor			=	:ll_Lote
		WHERE	plde_codigo			=	:li_Planta
		  AND	espe_codigo			=	:li_Lote_espcod
		  AND Upper(equi_nombre)=	Upper(:ls_nombrepc)
		USING	sqlca;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actualización Tabla Correlativos de Lotes Fruta Granel")
		Message.DoubleParm = -1
		SQLCA.AutoCommit	=	ib_AutoCommit
		RETURN FALSE
	END IF
	
END IF

RETURN TRUE
end function

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean procesopacking ();integer 	li_clie_codigo, li_i, li_hora, li_mins, li_mfge_numero
date 		ld_date_actual

//---------------variables para spro_ordenprocvacdeta
String 	ls_cale_calida
Decimal	ld_KilosRecep, ld_Tara, ld_NetoOriginal, ld_Neto, ld_KilosPromedio
time 		lt_hora_ivaciado, lt_hora_tvaciado

//---------------variables para lotefrutagranel
integer 	li_vari_codigo, li_prod_codigo

//---------------variables para lotefrutagrandeta
integer 	li_plde_codigo, li_lote_espcod, li_lote_codigo, li_enva_tipoen, &
		  	li_enva_codigo, li_lotd_totbul, li_lotd_totnet, li_lotd_kilpro
			  
//---------------variables para orden de proceso 
integer 	li_orpr_tipoord, li_orpr_numero

//---------------variables para programa de proceso 
integer 	li_ppre_numero, li_ppre_feccre

li_clie_codigo 		= 	dw_2.Object.clie_codigo[1]
li_mfge_numero 	= 	dw_2.Object.mfge_numero[1]

li_plde_codigo 		= 	dw_6.Object.lote_pltcod[1]
li_lote_espcod		= 	dw_6.Object.lote_espcod[1]
li_lote_codigo		= 	dw_6.Object.lote_codigo[1]
li_enva_tipoen		= 	dw_6.Object.enva_tipoen[1]
li_enva_codigo		= 	dw_6.Object.enva_codigo[1]
li_lotd_totbul		= 	dw_6.Object.lotd_totbul[1]
li_lotd_totnet		= 	dw_6.Object.lotd_totnet[1]
li_lotd_kilpro		= 	dw_6.Object.lotd_kilpro[1]

SELECT	vari_codigo, prod_codigo, lote_totnet
	INTO	:li_vari_codigo, :li_prod_codigo, :ld_kilosRecep
	FROM	dbo.spro_lotesfrutagranel as lote
	WHERE 	lote.lote_pltcod 	= :li_plde_codigo
		AND 	lote.lote_espcod	= :li_lote_espcod
		AND 	lote.lote_codigo	= :li_lote_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla LotesFrutaGranel")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Encabezado del lote no ha sido generado, se aborta el proceso. Ingrese Otro.")
	Return FALSE
END IF

ld_date_actual 		= 	DATE(STRING(TODAY(), "dd/mm/yyyy"))
li_hora 				= 	Hour(now())
li_mins 				= 	Minute(now())
lt_hora_ivaciado 	= 	time(li_hora, li_mins,0)
lt_hora_tvaciado 	= 	time(li_hora + 1, li_mins,0)

//*********************************** tablas de encabezados

INSERT INTO dbo.spro_movtofrutagranenca
		( clie_codigo, plde_codigo, tpmv_codigo,
		  mfge_numero, tran_codigo, espe_codigo,
		  cami_clasifi, cami_patent, cami_patcar,
		  mfge_fecmov, mfge_estmov, mfge_guisii,
		  mfge_chofer, mfge_observ, mfge_tpneto,
		  mfge_totbul, prod_codigo, sepl_codigo,
		  refg_horaen, refg_horasa, refg_tkbent,
		  refg_tkenen, refg_tkbsal, refg_tkensa,
		  refg_guienv, refg_moenen, refg_mosaen,
		  refg_diagra, moti_codigo, defg_tipdoc,
		  defg_docrel, mfge_dirdes, mfge_despac,
		  mfge_avisar, mfge_rutcho, refg_tkbenc,
		  refg_tkbsac, refg_tipcom, refg_descas,
		  rfge_kenrom, rfge_kenroc, rfge_ksarom,
		  rfge_ksaroc, mfge_usuari, mfge_comput,
		  mfge_horacr, mfge_usumod, mfge_commod,
		  mfge_horact, refg_fecsal, plde_coorde )
VALUES ( :li_clie_codigo, :li_plde_codigo, 21,
		  :li_mfge_numero, null, :li_lote_espcod,
		  null, null, null,
		  :ld_date_actual, 1, null,
		  null, null, null,
		  null, :li_prod_codigo, null,
		  null, null, null,
		  null, null, null,
		  null, null, null,
		  null, null, 4,
		  :li_lote_codigo, null, null,
		  null, null, null,
		  null, null, null,
		  null, null, null,
		  null, null, null,
		  null, null, null,
		  null, null, :li_plde_codigo );
		  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranenca")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF


INSERT INTO dbo.spro_movtoenvaenca  
		( clie_codigo, plde_codigo, tpmv_codigo,   
		  meen_numero, plde_coorde, prod_codigo, 
		  tran_codigo, cami_clasifi, cami_patent,
		  tpmv_codrec, mfge_numero, cami_patcar,
		  meen_guisii, meen_fecmov, meen_dirdes,
		  meen_despac, meen_avisar, meen_rutcho,
		  meen_chofer, meen_usuari, meen_comput,
		  meen_horacr, meen_usumod, meen_commod,
		  meen_horact, clpr_rut, meen_modulo )
VALUES ( :li_clie_codigo, :li_plde_codigo, 21,
		  :li_lote_codigo, :li_plde_codigo, :li_prod_codigo,
		  null, null, null,
		  21, :li_mfge_numero, null,
		  null, :ld_date_actual, null,
		  null, null, null,
		  null, null, null,
		  null, null, null,
		  null, null, null );
		  
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtoenvaenca")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF

INSERT INTO dbo.spro_movtofrutaembaenca  
		( clie_codigo, plde_codigo, tpmv_codigo,   
		  mfee_numero, mfee_fecmov, plde_coorde,   
		  tran_codigo, cami_clasifi, cami_patent,   
		  cami_patcar, mfee_chofer, mfee_tipdoc,   
		  mfee_docrel, moti_codigo, puer_codigo,   
		  embq_codigo, mfee_guides, mfee_horsal,   
		  mfee_horarr, etiq_codigo, etiq_codnue,   
		  expo_codigo, mfee_nturno, mfee_plasag,   
		  mfee_termog, mfee_observ, mfee_cantar,   
		  mfee_tardef, mfee_cancaj, mfee_cajmer,   
		  mfee_usuari, mfee_comput, mfee_horacr,   
		  mfee_usumod, mfee_commod, mfee_horact,   
		  tica_codigo, mfee_rutcho, pcfl_numero,   
		  line_codigo, mfee_palins )  
VALUES ( :li_clie_codigo, :li_plde_codigo, 4,   
		  :li_lote_codigo, :ld_date_actual, null,   
		  null, null, null,   
		  null, null, 4,   
		  :li_lote_codigo, null, null,   
		  null, null, null,   
		  null, null, null,   
		  1, null, null,   
		  null, null, null,   
		  null, null, null,   
		  null, null, null,   
		  null, null, :lt_hora_ivaciado,   
		  null, null, null,   
		  null, null )  ;
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutaembaenca")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF

//*********************************** tablas de detalles
For Li_i = 1 to dw_6.RowCount()	
	li_hora = Hour(now())
	li_mins = Minute(now())
	lt_hora_ivaciado = time(li_hora, li_mins + li_i,0)
	lt_hora_tvaciado = time(li_hora + 1, li_mins + li_i,0)
	
	li_plde_codigo = dw_6.Object.lote_pltcod[li_i]
	li_lote_espcod	= dw_6.Object.lote_espcod[li_i]
	li_lote_codigo	= dw_6.Object.lote_codigo[li_i]
	li_enva_tipoen	= dw_6.Object.enva_tipoen[li_i]
	li_enva_codigo	= dw_6.Object.enva_codigo[li_i]
	li_lotd_totbul	= dw_6.Object.lotd_totbul[li_i]
	li_lotd_totnet	= dw_6.Object.lotd_totnet[li_i]
	li_lotd_kilpro	= dw_6.Object.lotd_kilpro[li_i]
	
	INSERT INTO dbo.spro_ordenproceso  
		( clie_codigo, plde_codigo, orpr_tipord,   
		  orpr_numero, orpr_fecpro, prod_codigo,   
		  espe_codigo, vari_codigo, line_codigo,   
		  frio_tipofr, pefr_codigo, ppre_numero,   
		  orpr_nrotur, orpr_estado, orpr_canbul,   
		  orpr_bulpro, orpr_niveld, orpr_tippro,   
		  ppre_feccre )  
	VALUES (:li_clie_codigo, :li_plde_codigo, 4,   
	  :li_lote_codigo, :ld_date_actual, :li_prod_codigo,   
	  :li_lote_espcod, :li_vari_codigo, 1,   
	  '1', 1, 1,   //datos genericos
	  1, 1, :li_lotd_totbul,   
	  null, 1, 1, null )  ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenproceso")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
	
	INSERT INTO dbo.spro_ordenprocvacenca  
		( plde_codigo, orpr_tipord, orpr_numero,   
		  clie_codigo, opve_fecvac, opve_turno,   
		  opve_estado, line_codigo )  
	VALUES ( :li_plde_codigo, 4, :li_lote_codigo,   
	  :li_clie_codigo, :ld_date_actual, 1,   
	  'V', 1 )  ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacenca")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
		
	
	IF li_clie_codigo = 81 THEN
		ls_cale_calida = "1"
	ELSE
		ls_cale_calida = "2"
	END IF
	
	
	INSERT INTO dbo.spro_terminoproceso  
			( plde_codigo, tpmv_codigo, tepr_numero,   
			  tepr_secuen, lote_pltcod, lote_espcod,   
			  lote_codigo, lote_secuen, enva_tipoen,   
			  enva_codigo, clie_codigo, tepr_bultra,   
			  tepr_bulvac )  
	VALUES (:li_plde_codigo, 21, :li_lote_codigo,   
			  1, :li_plde_codigo, :li_lote_espcod,   
			  :li_lote_codigo, :Li_i, :li_enva_tipoen,   
			  :li_enva_codigo, :li_clie_codigo, :li_lotd_totbul,   
			  :li_lotd_totbul )  ;
			  
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_terminoproceso")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF

	INSERT INTO dbo.spro_movtofrutagrandeta  
         ( plde_codigo, tpmv_codigo, mfge_numero,   
           clie_codigo, mfgd_secuen, plde_coorde,   
           cama_codigo, lote_pltcod, lote_espcod,   
           lote_codigo, enva_tipoen, enva_codigo,   
           mfgd_bulent, mfgd_kgnent )  
  	VALUES ( :li_plde_codigo, 21, :li_mfge_numero,   
           :li_clie_codigo, :Li_i, :li_plde_codigo,   
           1, :li_plde_codigo, :li_lote_espcod,   
           :li_lote_codigo, :li_enva_tipoen, :li_enva_codigo,   
           :li_lotd_totbul, :li_lotd_totnet )  ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagrandeta")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF

	
	INSERT INTO dbo.spro_ordenprocdeta  
		( plde_codigo, orpr_tipord, orpr_numero,   
		  clie_codigo, orpd_secuen, lote_pltcod,   
		  lote_espcod, lote_codigo, enva_tipoen,   
		  enva_codigo, cama_codigo, orpd_nroban,   
		  orpd_nropos, orpd_nropis, orpd_canbul,   
		  orpd_canvac, orpd_horini, orpd_horfin,   
		  orpd_observ )  
	VALUES (:li_plde_codigo, 4, :li_lote_codigo,   
	  :li_clie_codigo, :Li_i, :li_plde_codigo,   
	  :li_lote_espcod, :li_lote_codigo, :li_enva_tipoen,   
	  :li_enva_codigo, 1, null,   
	  null, null, :li_lotd_totbul,   
	  null, null, null,   
	  null )  ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocdeta")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
	ld_NetoOriginal		=	ld_KilosRecep
	ld_Neto				=	ld_NetoOriginal
	ld_KilosPromedio	=	ld_Neto / li_lotd_totbul
	ld_KilosRecep		=	ld_KilosRecep + ( ld_tara * li_lotd_totbul )
	
	INSERT INTO dbo.spro_ordenprocvacdeta  
		( plde_codigo, orpr_tipord, orpr_numero,   
		  clie_codigo, opve_fecvac, opve_turno,   
		  opvd_horava, opvd_horate, lote_pltcod,   
		  lote_espcod, lote_codigo, enva_tipoen,   
		  enva_codigo, cale_calida, opvd_canbul,
		  opvd_kilpro, opvd_kilori, opvd_pesobr, opvd_pesone)  
	VALUES ( :li_plde_codigo, 4, :li_lote_codigo,   
	  :li_clie_codigo, :ld_date_actual, 1,   
	  :lt_hora_ivaciado, :lt_hora_tvaciado, :li_plde_codigo,   
	  :li_lote_espcod, :li_lote_codigo, :li_enva_tipoen,   
	  :li_enva_codigo, :ls_cale_calida, :li_lotd_totbul,
	  :ld_KilosPromedio, :ld_NetoOriginal, :ld_KilosRecep, :ld_Neto)  ;
	  
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacdeta")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF

  	UPDATE dbo.spro_movtofrutagranenca 
   SET mfge_estmov = 2  
   WHERE ( dbo.spro_movtofrutagranenca.plde_codigo = :li_plde_codigo ) AND  
         ( dbo.spro_movtofrutagranenca.clie_codigo = :li_clie_codigo ) AND  
         ( dbo.spro_movtofrutagranenca.tpmv_codigo = 21 ) AND  
         ( dbo.spro_movtofrutagranenca.mfge_numero = :li_lote_codigo );
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranenca")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF


	UPDATE dbo.spro_ordenproceso  
   SET orpr_estado = 2  
	WHERE ( dbo.spro_ordenproceso.plde_codigo = :li_plde_codigo ) AND  
         ( dbo.spro_ordenproceso.clie_codigo = :li_clie_codigo ) AND  
			( dbo.spro_ordenproceso.orpr_tipord = 4 ) AND  
         ( dbo.spro_ordenproceso.orpr_numero = :li_lote_codigo ) ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenproceso")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
NEXT 

RETURN TRUE
end function

public function boolean ordenproceso ();//w_maed_spro_orden_proceso 			lw_ordenp
//
//open(lw_ordenp)
//lw_ordenp.Visible = False
//
//lw_ordenp.dw_2.Object.ppre_numero[1] 	= 1//programa de proceso standard
//lw_ordenp.istr_mant.argumento[3] 		= 1//programa de proceso standard
//
//lw_ordenp.dw_2.Object.ppre_fecpro[1] 	= Date(Today())
//
//lw_ordenp.dw_2.Object.espe_codigo[1] 	= ii_especie
//lw_ordenp.istr_mant.argumento[2]			= ii_especie
//
Return True
end function

public subroutine asignapeso ();Long 		ll_Fila
Decimal 	ld_pesos = 0

IF dw_9.RowCount() > 0 THEN
	FOR ll_Fila = 1 TO dw_9.RowCount()
		ld_pesos =	ld_pesos + dw_9.Object.mfgp_pesore[ll_fila]
	NEXT
END IF

dw_2.Object.refg_tkbent[1]	 = ld_pesos
end subroutine

public function boolean existedocproceso (integer ai_planta, long al_numero);Long		ll_Numero,ll_Productor
Integer	li_Especie, li_Variedad, li_Cantidad, li_Vigencia, &
         li_periodo, li_linea, li_turno, li_Cliente
String   ls_Nombre, ls_frio
Boolean	lb_Retorno = True

li_Cliente	=	Integer(istr_mant.Argumento[10])
/*SELECT	op.orpr_numero, 	op.prod_codigo, 	op.espe_codigo, 
        op.vari_codigo, 	op.orpr_canbul, 	op.orpr_estado, 
    	op.frio_tipofr, 	op.pefr_codigo, 	op.orpr_nrotur, 
		op.line_codigo, 	va.vari_nombre,     va.vari_codigo,
		sum(ovd.opvd_kilori) as KiloProceso
FROM	dbo.spro_ordenproceso op, dbo.variedades va, dbo.spro_ordenprocvacdeta as ovd
WHERE	op.plde_codigo	=	1
	And	op.orpr_tipord	=	8
	And op.orpr_numero	=	104
    AND op.clie_codigo  =   81
	And	va.espe_codigo	=   op.espe_codigo
	And	va.vari_codigo	=	op.vari_codigo
    and op.plde_codigo	=	ovd.plde_codigo
    And	op.orpr_tipord	=	ovd.orpr_tipord
	And op.orpr_numero	=	ovd.orpr_numero
    AND op.clie_codigo  =   ovd.clie_codigo
Group by op.orpr_numero, 	op.prod_codigo, 	op.espe_codigo, 
        op.vari_codigo, 	op.orpr_canbul, 	op.orpr_estado, 
    	op.frio_tipofr, 	op.pefr_codigo, 	op.orpr_nrotur, 
		op.line_codigo, 	va.vari_nombre,     va.vari_codigo;
		*/
SELECT	op.orpr_numero, 
			op.prod_codigo, 
			op.espe_codigo, 
			op.vari_codigo, 
			op.orpr_canbul, 
			op.orpr_estado, 
			op.frio_tipofr, 
			op.pefr_codigo, 
			op.orpr_nrotur, 
			op.line_codigo, 
			va.vari_nombre
			
  INTO	:ll_Numero, 
  			:ll_Productor, 
			:li_Especie, 
			:li_Variedad, 
			:li_Cantidad, 
  			:li_Vigencia, 
			:ls_frio, 
			:li_periodo, 
			:li_turno, 
			:li_linea, 
			:ls_Nombre
			
	FROM	dbo.spro_ordenproceso op, 
			dbo.variedades va
			
	WHERE	op.plde_codigo	=	:ai_Planta
	And	op.orpr_tipord	=	8
	And   op.orpr_numero	=	:al_Numero 
	And	va.espe_codigo	=	op.espe_codigo
	And	va.vari_codigo	=	op.vari_codigo
//	And   va.clie_codigo =  op.clie_codigo
	AND   op.clie_codigo =  :li_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Ordenes de Proceso")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
//	IF Not ExisteMovtoProceso(ai_Planta,Integer(istr_mant.argumento[2]),4,ll_Numero) THEN
		istr_mant4.Argumento[5] 	=	String(ll_Numero)
		istr_mant4.Argumento[6]  	= 	String(ll_Productor)
		istr_mant4.Argumento[8]  	= 	String(li_Especie)
		istr_mant4.Argumento[9]  	= 	String(li_Variedad)
		istr_mant4.Argumento[10]	= 	ls_Nombre
		istr_mant4.Argumento[11] 	= 	String(li_Cantidad)
		istr_mant4.Argumento[12] 	=  	ls_frio
		istr_mant4.Argumento[13] 	=  	String(li_periodo)
		istr_mant4.Argumento[14] 	= 	String(li_turno)
		
//		dw_2.Setitem(1, "prod_codigo", Long(istr_mant.argumento[6]))
		dw_2.Object.espe_codigo[1]	=	li_Especie
//		dw_2.Setitem(1, "vari_codigo", Integer(istr_mant.argumento[9]))
//		dw_2.Setitem(1, "vari_nombre", istr_mant.argumento[10])
//		dw_2.SetItem(1, "frio_tipofr", istr_mant.argumento[12])
//		dw_2.SetItem(1, "pefr_codigo", Integer(istr_mant.argumento[13]))
//		dw_2.Setitem(1, "orpr_nrotur", Integer(istr_mant.argumento[14]))
//		dw_2.SetItem(1, "line_codigo", li_Linea)
		dw_2.Object.clie_codigo[1]		=	li_Cliente
//	END IF 
ELSE
	MessageBox("Atención","Número de Orden de Proceso No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean existemovtoproceso (integer ai_planta, integer ai_tipomovto, integer ai_tipodocto, long al_proceso);Long		ll_Numero
Boolean	lb_Retorno = True
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.Argumento[10])

SELECT	mfge_numero 
	INTO	:ll_Numero
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	20
	AND	defg_tipdoc	=	8
	AND	defg_docrel	=	:al_Proceso
	AND   clie_codigo =  :li_Cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Despachos Fruta Granel")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
//	istr_mant.Argumento[3]	=	String(ll_Numero)
//	This.TriggerEvent("ue_recuperadatos")
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine f_membreteds (datastore ds_1);ds_1.Modify("raz_social.text = '" + gstr_apl.Razon_Social + "'")
ds_1.Modify("nom_empresa.text = '" + gstr_apl.Nom_Empresa + "'")
ds_1.Modify("rut_empresa.text = 'R.U.T. " + Trim( String( Double( Mid( gstr_apl.Rut_Empresa , 1, 9 ) ), &
				'###,###,###') + '-' + Mid(gstr_apl.Rut_Empresa,10,1) + "'"))
ds_1.Modify("dir_empresa.text = '" + gstr_apl.Dir_Empresa + "'")
ds_1.Modify("tel_empresa.text = 'FONO " + gstr_apl.Tel_Empresa + "   FAX " + gstr_apl.Fax_Empresa + "'")
ds_1.Modify("ciu_empresa.text = '" + Trim(gstr_apl.Ciu_Empresa) + " / " + Trim(gstr_apl.Com_Empresa) + "'")
ds_1.Modify("gir_empresa.text = '" + gstr_apl.Gir_Empresa + "'")
ds_1.Modify("referencia.text = '" + gstr_apl.Referencia + "'")
ds_1.Modify("oficina.text = '" + gstr_apl.Oficina + "'")
ds_1.Modify("replegal.text = '" + gstr_apl.Rep_Legal + "'")
ds_1.Modify("rut_replegal.text = '" + Trim( String( Double( Mid( gstr_apl.Rut_RepLegal , 1, 9 ) ), &
				'###,###,###') + '-' + Mid(gstr_apl.Rut_RepLegal, 10, 1) + "'"))

RETURN
end subroutine

public function boolean conexionexistencia ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ib_ConectadoExistencia THEN
	DISCONNECT USING sqlexi;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT cone_nomodb,cone_nomser,cone_nombas,
		cone_nodbms,cone_nomusu,cone_passwo  
 INTO :ls_nomodb,:ls_nomser,:ls_nombas,
		:ls_nodbms,:ls_Usuario,:ls_Password
 FROM dbo.prodconectividad   
WHERE cone_codigo = :il_coneccion;

sqlexi.ServerName	=	ls_nomser
sqlexi.DataBase	=	ls_nombas
sqlexi.Dbms			= 	ls_nodbms
sqlexi.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
CONNECT USING sqlexi;

IF sqlexi.SQLCode = 0 THEN
	ib_ConectadoExistencia	=	True
ELSE
	ib_ConectadoExistencia	=	False
END IF

RETURN ib_ConectadoExistencia

end function

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, long al_numero, integer ai_cliente);Long		ll_Numero
Boolean	lb_Retorno = True

IF NOT gb_RecepcionDeProceso THEN
		
	SELECT	mfge_numero
		INTO	:ll_Numero
		FROM	dbo.spro_movtofrutagranenca
		WHERE	plde_codigo	=	:ai_Planta
		AND	tpmv_codigo	=	:ai_TipoMovto
		AND	mfge_numero	=	:al_Numero
		AND   clie_codigo 		=  :ai_Cliente
		AND   IsNull(defg_tipdoc, -1) = -1;
ELSE
	
	SELECT	mfge_numero
		INTO	:ll_Numero
		FROM	dbo.spro_movtofrutagranenca
		WHERE	plde_codigo	=	:ai_Planta
		AND	tpmv_codigo	=	:ai_TipoMovto
		AND	mfge_numero	=	:al_Numero
		AND   clie_codigo 		=  :ai_Cliente
		AND   IsNull(defg_tipdoc, -1) = 8;
END IF
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(al_Numero)
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado o ~r~nNo es una " + Lower(This.Title) + ". Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine updatemovtogranpesa ();Long		ll_fila, ll_FindLote
Decimal	ld_kilpro
uo_bins	luo_bins
luo_bins	=	Create uo_bins

FOR ll_fila = 1 TO dw_9.RowCount()
	
	ll_FindLote	=	dw_3.Find("lote_codigo = " + String(dw_9.Object.lote_codigo[ll_fila]), 1, dw_3.RowCount())
	
	IF ll_FindLote	> 0 THEN
		
		iuo_bins.Existe(dw_2.Object.clie_codigo[1],&
							 dw_2.Object.plde_codigo[1],&
							 dw_9.Object.bins_numero[ll_fila], sqlca, TRUE)
		
		ld_kilpro								=	dw_3.Object.lote_kilpro[ll_FindLote]
		
		IF gstr_paramplanta.aplicaporc = 1 AND dw_2.Object.espe_codigo[1] = 21 THEN
			luo_bins.Existe(dw_2.Object.clie_codigo[1],&
								 dw_2.Object.plde_codigo[1],&
								 dw_9.Object.mfgp_tibapa[ll_fila], sqlca, TRUE)
		ELSE
			luo_bins.cale_pesoen	=	0
		END IF
		
		dw_9.Object.mfgp_pesore[ll_fila]	=	Dec(ld_kilpro + iuo_bins.cale_pesoen + (luo_bins.cale_pesoen / dw_3.Object.lote_totbul[ll_FindLote]))
		dw_9.Object.mfgp_valref[ll_fila]	=	Dec(dw_9.Object.mfgp_pesore[ll_fila] * dw_3.Object.lote_totbul[ll_FindLote])
		
	END IF
	
NEXT
end subroutine

public function boolean chequea_binscontraenvases ();Integer	li_fila, li_str, li_tipenv, li_codenv, li_cantid
Long		ll_prod[]
String	ls_calcos, ls_mensaje
boolean	lb_Respuesta = TRUE, lb_existe
str_envase	lstr_env[]
DataStore	lds_bins
uo_bins		luo_bins

luo_bins						=	Create uo_bins
lds_bins						=	Create DataStore
lds_bins.DataObject		=	dw_spro_bins.DataObject

dw_spro_bins.RowsCopy(1, dw_spro_bins.RowCount(), Primary!, lds_bins, 1, Primary!)
lds_bins.SetSort("enva_tipoen asc, enva_codigo asc, cale_calida asc")
lds_bins.Sort()

IF lds_bins.RowCount() 	= 	0 THEN RETURN lb_respuesta

li_str						=	1
li_fila						=	1
lstr_env[li_str].tipenv = 	lds_bins.Object.enva_tipoen[li_fila]
lstr_env[li_str].codenv = 	lds_bins.Object.enva_codigo[li_fila]
lstr_env[li_str].calcos = 	lds_bins.Object.cale_calida[li_fila]

FOR li_fila = 1 TO dw_3.RowCount()
	ll_prod[li_fila]	=	dw_3.Object.prod_codigo[li_fila]
NEXT

FOR li_fila = 1 TO lds_bins.RowCount()
	
	IF NOT luo_bins.Existe(lds_bins.Object.clie_codigo[li_fila],&
					 			  lds_bins.Object.plde_codigo[li_fila],&
							 	  lds_bins.Object.bins_numero[li_fila],sqlca,True) THEN
		RETURN FALSE
	END IF
	
	IF lstr_env[li_str].tipenv <> luo_bins.enva_tipoen OR &
		lstr_env[li_str].codenv <> luo_bins.enva_codigo OR &
		lstr_env[li_str].calcos <> luo_bins.cale_calida THEN
		
		li_str						=	li_str + 1
		lstr_env[li_str].tipenv = 	luo_bins.enva_tipoen
		lstr_env[li_str].codenv = 	luo_bins.enva_codigo
		lstr_env[li_str].calcos = 	luo_bins.cale_calida
		
		IF NOT gstr_paramplanta.palletdebins THEN
			lstr_env[li_str].cantid = 	1
		ELSE
			lstr_env[li_str].cantid =	lds_bins.Object.fgmb_canbul[li_fila]
		END IF
		
	ELSE
		
		IF NOT gstr_paramplanta.palletdebins THEN
			lstr_env[li_str].cantid =	lstr_env[li_str].cantid + 1
		ELSE
			lstr_env[li_str].cantid =	lstr_env[li_str].cantid + lds_bins.Object.fgmb_canbul[li_fila]
		END IF
		
	END IF

NEXT

lds_bins.SetSort("fgmb_tibapa asc")
lds_bins.Sort()

FOR li_fila = 1 TO lds_bins.RowCount()
	
	IF NOT luo_bins.Existe(lds_bins.Object.clie_codigo[li_fila],&
					 			  lds_bins.Object.plde_codigo[li_fila],&
							 	  lds_bins.Object.fgmb_tibapa[li_fila],sqlca,True) THEN
		RETURN FALSE
	END IF
	
	IF lstr_env[li_str].tipenv <> luo_bins.enva_tipoen OR &
		lstr_env[li_str].codenv <> luo_bins.enva_codigo OR &
		lstr_env[li_str].calcos <> luo_bins.cale_calida THEN
		
		li_str						=	li_str + 1
		lstr_env[li_str].tipenv = 	luo_bins.enva_tipoen
		lstr_env[li_str].codenv = 	luo_bins.enva_codigo
		lstr_env[li_str].calcos = 	luo_bins.cale_calida
		lstr_env[li_str].cantid = 	1
		
	ELSE
		lstr_env[li_str].cantid =	lstr_env[li_str].cantid + 1
		
	END IF
NEXT


FOR li_fila = 1 TO dw_5.RowCount()
	
	li_tipenv	=	dw_5.Object.enva_tipoen[li_fila]
	li_codenv 	= 	dw_5.Object.enva_codigo[li_fila]
	ls_calcos 	= 	dw_5.Object.cale_calida[li_fila]
	li_cantid	=	dw_5.Object.fgme_cantid[li_fila]
	
	FOR li_str = 1 TO UpperBound(lstr_env)
		IF lstr_env[li_str].tipenv = li_tipenv AND &
			lstr_env[li_str].codenv = li_codenv AND &
			lstr_env[li_str].calcos = ls_calcos THEN
			
			lb_existe = TRUE
			IF lstr_env[li_str].cantid <> li_cantid THEN
				ls_mensaje = ls_mensaje + "* La cantidad de envases ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
				ls_mensaje = ls_mensaje + "ingresada no corresponde con los bins ingresados con esas caracteristicas.~r~n"
			END IF			
		END IF
	NEXT
	IF NOT lb_existe THEN
		ls_mensaje = ls_mensaje + "* Los "+String(li_cantid)+" envases ingresados ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
		ls_mensaje = ls_mensaje + "no corresponde a las caractisticas de ningún Bins ingresado.~r~n"
	END IF
	
	lb_existe = False
	FOR li_str = 1 TO UpperBound(ll_prod[])
		IF dw_5.Object.prod_codigo[li_fila]	=	ll_prod[li_str] THEN
			lb_existe = TRUE
		END IF
	NEXT
	IF NOT lb_existe THEN
		ls_mensaje = ls_mensaje + "* El productor ("+String(dw_5.Object.prod_codigo[li_fila])+") con envases ingresados ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
		ls_mensaje = ls_mensaje + "no corresponde a el o los productores de los lotes recepcionados.~r~n"
	END IF
	
	lb_existe = False
NEXT

IF Len(ls_mensaje) > 0 THEN
	lb_respuesta = False
	MessageBox("Error de Consistencia", "Se han detectado los siguientes errores con los envases ingresados:~r~n" + ls_mensaje, StopSign!)
ELSE
	lb_respuesta = True
END IF
RETURN lb_Respuesta
end function

public subroutine cargamovenv ();Integer	li_rectipenv, li_filadw_5

IF tab_1.tp_1.dw_lotes.RowCount() < 1 THEN RETURN

tab_1.tp_2.dw_envrec.RowsMove(1, tab_1.tp_2.dw_envrec.RowCount(), Primary!, tab_1.tp_2.dw_envrec, tab_1.tp_2.dw_envrec.RowCount(), Delete!)

FOR li_rectipenv = LowerBound(is_enva_tipoen) TO UpperBound(is_enva_tipoen)
	
	IF Integer(is_enva_tipoen[li_rectipenv]) > 0 THEN
		
		li_filadw_5 														= 	tab_1.tp_2.dw_envrec.InsertRow(0)
		tab_1.tp_2.dw_envrec.Object.prod_codigo[li_filadw_5]	=	tab_1.tp_1.dw_lotes.Object.prod_codigo[tab_1.tp_1.dw_lotes.GetRow()]
		tab_1.tp_2.dw_envrec.Object.prod_nombre[li_filadw_5]	=	tab_1.tp_1.dw_lotes.Object.prod_nombre[tab_1.tp_1.dw_lotes.GetRow()]
		tab_1.tp_2.dw_envrec.Object.plde_codigo[li_filadw_5]	=	dw_2.Object.plde_codigo[1]
		tab_1.tp_2.dw_envrec.Object.enva_tipoen[li_filadw_5]	=	Integer(is_enva_tipoen[li_rectipenv])
		tab_1.tp_2.dw_envrec.Object.enva_codigo[li_filadw_5]	=	Integer(is_enva_codigo[li_rectipenv])
		tab_1.tp_2.dw_envrec.Object.cale_calida[li_filadw_5]	=	is_cale_calida[li_rectipenv]
		tab_1.tp_2.dw_envrec.Object.fgme_cantid[li_filadw_5]	=	Integer(is_cantidad[li_rectipenv])
		tab_1.tp_2.dw_envrec.Object.fgme_pesone[li_filadw_5]	=	Dec(is_pesone[li_rectipenv])
		tab_1.tp_2.dw_envrec.Object.fgme_conenv[li_filadw_5]	=	1
		tab_1.tp_2.dw_envrec.Object.clie_codigo[li_filadw_5]	=	dw_2.Object.clie_codigo[1]
		tab_1.tp_2.dw_envrec.Object.cale_nombre[li_filadw_5]	=	is_cale_nombre[li_rectipenv]
		tab_1.tp_2.dw_envrec.Object.fgme_sentid[li_filadw_5]	=	1
	END IF
NEXT
end subroutine

public subroutine cargaenvases ();Long 			ll_Fila, ll_rectbp, ll_tibapa[], ll_cant_tibapa[], ll_tibapaant, ll_totalbultos
Decimal 		ld_pesos = 0
Integer		li_filas, li_desde, li_hasta, li_UseFind, li_recenv
String 		ls_enva_tipoen[], ls_enva_codigo[], ls_cale_calida[], ls_cantidad[], &
				ls_pesone[], ls_cale_nombre[], ls_prod_codigo[], ls_prod_nombre[]
Boolean		lb_flag, lb_flgtpb
DataWindow	dw_9Mirror, dw_binsmirror

dw_9.SetFilter('')
dw_9.Filter()

dw_9Mirror		=	Create DataWindow
dw_binsmirror	=	Create DataWindow

dw_9Mirror		=	dw_9
dw_binsmirror	=	dw_spro_bins

dw_9Mirror.SetSort("mfgp_tibapa asc, bins_numero asc")
dw_binsmirror.SetSort("mfgp_tibapa asc, bins_numero asc")
dw_9Mirror.Sort()
dw_binsmirror.Sort()

FOR ll_Fila  =  1 TO dw_binsmirror.RowCount()
	lb_flgtpb =	False
	
	FOR ll_rectbp = 1 TO UpperBound(ll_tibapa[])
		IF ll_tibapa[ll_rectbp] = dw_binsmirror.Object.fgmb_tibapa[ll_fila] THEN
			lb_flgtpb				=	True
			ll_cant_tibapa[ll_rectbp] ++
			EXIT
		END IF
	NEXT
	
	IF NOT lb_flgtpb THEN
		ll_tibapa[ll_rectbp+1]		=	dw_binsmirror.Object.fgmb_tibapa[ll_fila]
		ll_cant_tibapa[ll_rectbp+1]=	1
	END IF 
NEXT

IF dw_9Mirror.RowCount() > 0 THEN
	FOR ll_Fila = 1 TO dw_9Mirror.RowCount()
		
		//INICIO Control de envases
		iuo_bins.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_9Mirror.Object.bins_numero[ll_Fila], sqlca, TRUE)
		
		lb_flag	=	False
		IF UpperBound(ls_enva_tipoen[]) = 0 THEN
			ls_enva_tipoen[1]		=	String(iuo_bins.enva_tipoen)
			ls_enva_codigo[1]		=	String(iuo_bins.enva_codigo)
			ls_cale_calida[1]		=	iuo_bins.cale_calida
			ls_cantidad[1]			=	"1"
			ls_pesone[1]			=	String(iuo_bins.cale_pesoen)
			ls_cale_nombre[1]		=	iuo_bins.cale_nombre
			
		ELSE
			
			FOR li_recenv = LowerBound(ls_enva_tipoen[]) TO UpperBound(ls_enva_tipoen[])
				IF ls_enva_tipoen[li_recenv] = String(iuo_bins.enva_tipoen) THEN
					IF ls_enva_codigo[li_recenv] = String(iuo_bins.enva_codigo) THEN
						IF ls_cale_calida[li_recenv] = String(iuo_bins.cale_calida) THEN
						
							ls_cantidad[li_recenv]	=	String(Integer(ls_cantidad[li_recenv]) + 1)
							lb_flag						=	True
							EXIT
							
						END IF
					END IF
				END IF
			NEXT
			
			IF NOT lb_flag THEN
				li_recenv								=	li_recenv + 1
				ls_enva_tipoen[li_recenv]			=	String(iuo_bins.enva_tipoen)
				ls_enva_codigo[li_recenv]			=	String(iuo_bins.enva_codigo)
				ls_cale_calida[li_recenv]			=	iuo_bins.cale_calida
				ls_cantidad[li_recenv]				=	"1"
				ls_pesone[li_recenv]					=	String(iuo_bins.cale_pesoen)
				ls_cale_nombre[li_recenv]			=	iuo_bins.cale_nombre
			END IF
			
		END IF
		
		//FIN Control de envases
	NEXT
END IF

FOR ll_fila = 1 TO UpperBound(ls_cantidad[])
	ll_totalbultos	=	ll_totalbultos + Long(ls_cantidad[ll_fila])
NEXT
IF ll_totalbultos > 0 THEN
	dw_2.Object.mfge_totbul[1]	=	ll_totalbultos
END IF

IF UpperBound(ll_tibapa[]) > 0 THEN
	FOR ll_Fila = 1 TO UpperBound(ll_tibapa[])
		IF ll_tibapa[ll_Fila] > 0 THEN
			//INICIO Control de base pallets
			iuo_bins.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], &
								 ll_tibapa[ll_Fila], sqlca, TRUE)
			
			lb_flag	=	False
			IF UpperBound(ls_enva_tipoen[]) = 0 THEN
				ls_enva_tipoen[1]		=	String(iuo_bins.enva_tipoen)
				ls_enva_codigo[1]		=	String(iuo_bins.enva_codigo)
				ls_cale_calida[1]		=	iuo_bins.cale_calida
				ls_cantidad[1]			=	String(ll_cant_tibapa[ll_fila])//"1"
				ls_pesone[1]			=	String(iuo_bins.cale_pesoen)
				ls_cale_nombre[1]		=	iuo_bins.cale_nombre
				
			ELSE
				
				FOR li_recenv = LowerBound(ls_enva_tipoen[]) TO UpperBound(ls_enva_tipoen[])
					IF ls_enva_tipoen[li_recenv] = String(iuo_bins.enva_tipoen) THEN
						IF ls_enva_codigo[li_recenv] = String(iuo_bins.enva_codigo) THEN
							IF ls_cale_calida[li_recenv] = String(iuo_bins.cale_calida) THEN
							
								ls_cantidad[li_recenv]	=	String(Integer(ls_cantidad[li_recenv]) + ll_cant_tibapa[ll_fila])
								lb_flag						=	True
								EXIT
								
							END IF
						END IF
					END IF
				NEXT
				
				IF NOT lb_flag THEN
					li_recenv								=	li_recenv + 1
					ls_enva_tipoen[li_recenv]			=	String(iuo_bins.enva_tipoen)
					ls_enva_codigo[li_recenv]			=	String(iuo_bins.enva_codigo)
					ls_cale_calida[li_recenv]			=	iuo_bins.cale_calida
					ls_cantidad[li_recenv]				=	String(ll_cant_tibapa[ll_fila])
					ls_pesone[li_recenv]					=	String(iuo_bins.cale_pesoen)
					ls_cale_nombre[li_recenv]			=	iuo_bins.cale_nombre
				END IF
				
			END IF
			
			//FIN Control de envases
		END IF
	NEXT
END IF
/**/
//Asignación de envases recepcionados
is_enva_tipoen[]	=	ls_enva_tipoen[]
is_enva_codigo[]	=	ls_enva_codigo[]
is_cale_calida[]	=	ls_cale_calida[]
is_cantidad[]		=	ls_cantidad[]
is_pesone[]			=	ls_pesone[]
is_cale_nombre[]	=	ls_cale_nombre[]

CargaMovEnv()
end subroutine

public function boolean packingproductor (long al_productor);
SELECT 	MAX(prpk_bodega)
INTO		:il_packing
FROM 		dbo.prodpacking
WHERE		prod_codigo =:al_productor;

IF il_packing = 0 OR isnull(il_packing) THEN
	Return False
END IF

Return True
end function

public subroutine set_tarjas (integer cliente, long planta);Integer	li_existe

SELECT count(*)
  INTO :li_existe
  FROM dbo.spro_correltarjas
 WHERE clie_codigo = :cliente
   AND plde_codigo = :planta;
	
IF SQLCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SQLCa, "Lectura de Tablas ")
	
ELSEIF li_existe < 1 THEN
	MessageBox("Creación de Tarjas", "Se procedera a crear set de Tarjas " + &
												"para Cliente " + String(cliente, '000') + &
												" y Planta " + String(planta, '0000') + &
												".", Information!)
	INSERT INTO dbo.spro_correltarjas
			 (clie_codigo, plde_codigo, crta_numero, crta_ccajas)
	VALUES (:cliente, :planta, 1, 1);
	COMMIT;
END IF
end subroutine

public subroutine wf_bloqueacolumnas (boolean ab_bloquea);//
end subroutine

public function boolean datos_correo ();Integer li_codigo

SELECT 	empr_codplt
INTO		:li_codigo
FROM 		dbo.parempresa;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla parempresa")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

SELECT 	plde_correo
INTO		:is_correo
FROM 		dbo.plantadesp
WHERE 	plde_codigo = :li_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla parempresa")
	Return True
ELSEIF is_correo = '' OR Isnull(is_correo) THEN
	MessageBox("Atención","Falta Correo en Tabla Planta Código "+String(li_codigo))
	is_correo = '<pvaldes@rioblanco.cl>;<spison@rioblanco.cl>'
	Return True
END IF

Return False
end function

on w_maed_movtofrutagranel_recepcereza.create
int iCurrent
call super::create
this.cb_guia=create cb_guia
this.dw_4=create dw_4
this.dw_spro_bins=create dw_spro_bins
this.dw_9=create dw_9
this.dw_6=create dw_6
this.dw_exideta=create dw_exideta
this.dw_exiencab=create dw_exiencab
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
this.dw_exidetaborra=create dw_exidetaborra
this.dw_envases_comer=create dw_envases_comer
this.dw_lotescategoria=create dw_lotescategoria
this.ole_puerta=create ole_puerta
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_guia
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_spro_bins
this.Control[iCurrent+4]=this.dw_9
this.Control[iCurrent+5]=this.dw_6
this.Control[iCurrent+6]=this.dw_exideta
this.Control[iCurrent+7]=this.dw_exiencab
this.Control[iCurrent+8]=this.dw_exismovtodetanulos
this.Control[iCurrent+9]=this.dw_exidetaborra
this.Control[iCurrent+10]=this.dw_envases_comer
this.Control[iCurrent+11]=this.dw_lotescategoria
this.Control[iCurrent+12]=this.ole_puerta
this.Control[iCurrent+13]=this.tab_1
end on

on w_maed_movtofrutagranel_recepcereza.destroy
call super::destroy
destroy(this.cb_guia)
destroy(this.dw_4)
destroy(this.dw_spro_bins)
destroy(this.dw_9)
destroy(this.dw_6)
destroy(this.dw_exideta)
destroy(this.dw_exiencab)
destroy(this.dw_exismovtodetanulos)
destroy(this.dw_exidetaborra)
destroy(this.dw_envases_comer)
destroy(this.dw_lotescategoria)
destroy(this.ole_puerta)
destroy(this.tab_1)
end on

event open;/* Argumentos
istr_mant.argumento[1] = Código Planta
istr_mant.argumento[2] = Tipo de Movimiento
istr_mant.argumento[3] = Número de Despacho
istr_mant.argumento[4] = Sentido del Movimiento de Envases
istr_mant.argumento[5] = Especie
istr_mant.argumento[6] = Productor no se asigna / va en el detalle del lote
istr_mant.argumento[7] = Ultimo Lote Ocupado
istr_mant.argumento[8] = decimales especie ( 0 - 2 ) 
istr_mant.argumento[9] = Parametro para definir si es [M]antencion o [R]ecepcion
istr_mant.argumento[10] = Código Cliente
*/
Integer	li_resultado
String	ls_parametros, ls_nombre

x				= 0
y				= 0
This.Height	= 2500
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF gstr_ParamPlanta.etiquetaembalaje = 0 THEN
	tab_1.tp_1.dw_lotes.DataObject	=	"dw_mues_spro_lotesfrutagranel_rec_kguia"
END IF

dw_2.Object.oproceso.visible 		= 	gb_RecepcionDeProceso
dw_2.Object.defg_docrel.visible 	= 	gb_RecepcionDeProceso
dw_2.Object.ordenproceso.visible 	= 	gb_RecepcionDeProceso

IF NOT gb_RecepcionDeProceso THEN
	This.Title = "RECEPCION DE HUERTO"
ELSE
	This.Title = "RECEPCION DE PREPROCESO"
END IF

//ii_TipMov 								= 	1

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

//Descomentado por PMH, para evitar posibles problemas con pre proceso de limones
//30-01-07 ¿¿¿??? no se cuales son las lineas de codigo descomentadas.

IF gb_RecepcionDeProceso THEN
	istr_mant.argumento[2]	=	'1'
ELSE
	istr_mant.argumento[2]	=	'1'
END IF

istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[4]	=	'1'
istr_mant.argumento[5]	=	String(gstr_ParamPlanta.CodigoEspecie)
istr_mant.argumento[7]	=	'0'
istr_mant.argumento[9] 	=	'R'
istr_mant.argumento[10] =	String(gi_codexport)

dw_spro_bins.SetTransObject(sqlca)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve() 

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

IF idwc_Transp.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Transportistas")
	idwc_Transp.InsertRow(0)
ELSE
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()	
END IF

dw_1.GetChild("cama_codigo",idwc_Camara)
idwc_Camara.SetTransObject(SQLCA)
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.GetChild("lote_espcod", idwc_especiedet2)
idwc_especiedet2.SetTransObject(sqlca)
IF idwc_especiedet2.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especiedet2.InsertRow(0)
ELSE
	idwc_especiedet2.SetSort("espe_nombre A")
	idwc_especiedet2.Sort()
END IF

dw_3	=	tab_1.tp_1.dw_lotes
dw_5	=	tab_1.tp_2.dw_envrec
dw_7	=	tab_1.tp_3.dw_envdes

dw_3.GetChild("prbr_codpre",idwc_Predio)
idwc_Predio.SetTransObject(SQLCA)
idwc_Predio.InsertRow(0)

dw_3.GetChild("vari_codigo",idwc_Variedad)
idwc_Variedad.SetTransObject(SQLCA)
idwc_Variedad.Retrieve(gstr_ParamPlanta.CodigoEspecie)

dw_4.GetChild("plde_codigo",idwc_plantadw4)
idwc_plantadw4.SetTransObject(SQLCA)
idwc_plantadw4.Retrieve()

dw_4.GetChild("plde_coorde",idwc_plancodw4)
idwc_plancodw4.SetTransObject(SQLCA)
idwc_plancodw4.Retrieve()

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)

dw_1.GetChild("lote_pltcod",idwc_plantadw1)
idwc_plantadw1.SetTransObject(SQLCA)
idwc_plantadw1.Retrieve()

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 84")

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.Modify("DataWindow.Footer.Height = 84")

dw_5.Modify("datawindow.message.title='Error '+ is_titulo")
dw_5.Modify("DataWindow.Footer.Height = 84")

dw_7.Modify("datawindow.message.title='Error '+ is_titulo")
dw_7.Modify("DataWindow.Footer.Height = 84")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.solo_consulta			=	False

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
iuo_Transport			=	Create uo_transportista
iuo_Camion				=	Create uo_camiones
iuo_Especie				=	Create uo_especie
iuo_Correlativo			=	Create uo_LotesCorrel
iuo_PesoEstanEspe	=	Create uo_PesoEstanEspe
iuo_FechaMovto		=	Create uo_FechaMovto
iuo_tipomovtofruta		=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva	=	Create uo_tipomovtofruta
iuo_bins					=	Create uo_bins
iuo_calicosechero		=  Create uo_calicosechero  
end event

event resize;call super::resize;Tab_1.x								=	dw_1.x
Tab_1.y								=	dw_1.y
Tab_1.Width						=	dw_1.Width
Tab_1.Height						=	dw_1.Height

Tab_1.Tp_1.dw_lotes.x			= 27
Tab_1.Tp_1.dw_lotes.y			= 36
Tab_1.Tp_1.dw_lotes.height	= Tab_1.height - 180
Tab_1.Tp_1.dw_lotes.width		= Tab_1.width - 92

Tab_1.Tp_2.dw_envrec.x		= 27
Tab_1.Tp_2.dw_envrec.y		= 36
Tab_1.Tp_2.dw_envrec.height	= Tab_1.height - 180
Tab_1.Tp_2.dw_envrec.width	= Tab_1.width - 92

Tab_1.Tp_3.dw_envdes.x		= 27
Tab_1.Tp_3.dw_envdes.y		= 36
Tab_1.Tp_3.dw_envdes.height	= Tab_1.height - 180
Tab_1.Tp_3.dw_envdes.width	= Tab_1.width - 92

cb_guia.x 							=  pb_salir.x
cb_guia.y 							=  pb_salir.y + pb_salir.Height + 10
end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_tabpage, li_borra
Boolean	lb_estado
str_mant_envases	lstr_mant

li_tabpage	=	tab_1.SelectedTab 

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1 
		IF NOT ib_Destare THEN
			istr_mant.dw	=	dw_3
			istr_mant.dw2	=	dw_6
			OpenWithParm(iw_mantencion_1, istr_mant)
		ELSE
			RETURN
		END IF
	CASE 2
		IF NOT ib_Destare THEN
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'1'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		ELSE
			RETURN
		END IF
	CASE 2
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'2'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		
END CHOOSE

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	CHOOSE CASE li_tabpage
		CASE 1
			li_borra	=	dw_3.DeleteRow(0)
			IF li_Borra = 1 THEN
				li_Borra = dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
			END IF
		CASE 2
			li_borra	=	dw_5.DeleteRow(0) 
		CASE 3
			li_borra	=	dw_7.DeleteRow(0)
	END CHOOSE
 
	IF li_borra = 1 THEN
		dw_6.SetFilter("")
		dw_6.Filter()
		
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_3.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False

captura_totales()
CargaEnvases()
end event

event ue_borrar;Integer		li_Cliente, li_aplicaenvase

li_Cliente	=	dw_2.Object.clie_codigo[1]

IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

il_numeroenva = dw_4.Object.meen_numero[1]

IF Message.DoubleParm = -1 THEN RETURN

li_Cliente	=	dw_2.Object.clie_codigo[1]

SELECT 	clie_conexi, cone_codigo
INTO   	:il_conexiste, :il_coneccion
FROM dbo.clientesprod
WHERE clie_codigo = :li_Cliente;

IF il_conexiste = 1 THEN
	sqlexi	=	CREATE Transaction
	
	IF Conexionexistencia() THEN
		dw_exideta.SetTransObject(sqlexi)
		dw_exiencab.SetTransObject(sqlexi)	
		dw_exismovtodetanulos.SetTransObject(sqlexi)
		dw_exidetaborra.SetTransObject(sqlexi)
		TriggerEvent("anulamovto")
		DISCONNECT USING sqlexi;
		
		SELECT isnull(plde_renenv,0)
			INTO :li_aplicaenvase
			FROM "dbo"."plantadesp" 
			WHERE plde_codigo = :il_packing;
		
		IF li_aplicaenvase = 1 THEN
			IF Conexionexistencia() THEN
				dw_exideta.SetTransObject(sqlexi)
				dw_exiencab.SetTransObject(sqlexi)	
				dw_exismovtodetanulos.SetTransObject(sqlexi)
				dw_exidetaborra.SetTransObject(sqlexi)
				TriggerEvent("anulamovto2")
				
				DISCONNECT USING sqlexi;
				
			ELSE
				MessageBox("Atención", "Imposible Conectar con Base de Existencia(Generar Despacho).",Exclamation!, OK!)
			END IF
		END IF	
	ELSE
		MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
	END IF
END IF	

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
IF dw_6.RowCount() > 0 THEN dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
IF dw_7.RowCount() > 0 THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
IF dw_9.RowCount() > 0 THEN dw_9.RowsMove(1,dw_9.RowCount(),Primary!,dw_9,1,Delete!)
IF dw_spro_bins.RowCount() > 0 THEN dw_spro_bins.RowsMove(1,dw_spro_bins.RowCount(),Primary!,dw_spro_bins,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
	ib_borrar = False
	w_main.SetMicroHelp("Borrando Registro...")
	
	ib_AutoCommit		=	SQLCA.AutoCommit
	SQLCA.AutoCommit	=	False
	
	IF wf_actualiza_db(True) THEN
		w_main.SetMicroHelp("Registro Borrado...")
		sqlexi		=	CREATE Transaction
				
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

event ue_guardar;Integer	li_Cliente, li_aplicaenvase
String	ls_numero
Date		ld_fecha

IF ib_destare and gstr_paramplanta.packing THEN pb_grabar.Enabled = FALSE

IF dw_3.AcceptText() = -1 THEN RETURN
IF dw_4.AcceptText() = -1 THEN RETURN
IF dw_5.AcceptText() = -1 THEN RETURN
IF dw_1.AcceptText() = -1 THEN RETURN

ls_numero = String(dw_2.Object.mfge_numero[1])
ld_fecha = dw_2.Object.mfge_fecmov[1]

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	
	//Carga Memo Errores
	If MessageBox("Atencion", "Desea Cargar Memo de Errores.", Exclamation!, YesNo!, 2) = 1 Then 	
		F_CargaMemoError(dw_2, 2, '1')
	End If
	
	//Crea conexion hacia existencia
	
	IF NOT gb_RecepcionDeProceso THEN
		IF dw_2.Object.mfge_estmov[1] = 3 THEN
			li_Cliente	=	dw_2.Object.clie_codigo[1]
			
			SELECT 	clie_conexi, cone_codigo
			INTO   	:il_conexiste, :il_coneccion
			FROM dbo.clientesprod
			WHERE clie_codigo = :li_Cliente;
				
			IF il_conexiste = 1 THEN
				sqlexi	=	CREATE Transaction
				iuo_grabatablabitacora			=	Create uo_grabatablabitacora
				Datos_correo()
				
				IF Conexionexistencia() THEN
					dw_exideta.SetTransObject(sqlexi)
					dw_exiencab.SetTransObject(sqlexi)	
					dw_exismovtodetanulos.SetTransObject(sqlexi)
					dw_exidetaborra.SetTransObject(sqlexi)
					TriggerEvent("ue_despuesborrar")
					TriggerEvent("ue_despuesguardar")
					
					IF li_retorno = 2 THEN
						iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
						'Verificar Datos en Base de Existencia '+is_base,is_base,1,is_error,1,il_coderror,&
						is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
						is_error = ''	
					END IF		
					
					DISCONNECT USING sqlexi;
					
					IF isnull(il_packing) OR il_packing = 0 THEN
						iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
						'Verificar Packing del Productor '+is_base,is_base,1,'Falta Packing Productor',1,0,&
						is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+'Falta Packing Productor')
						is_error = ''	
						
						IF Message.DoubleParm = -1 THEN RETURN

						IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
							il_NumFruta = 0
							il_NumEnva	= 0
						END IF
						
						IF dw_2.Object.mfge_estmov[1]	=	3 THEN
							ActualizaKilosLote()
						END IF
						Return	
					END IF	
					
					IF li_retorno <> 2 THEN
						SELECT isnull(plde_renenv,0)
							INTO :li_aplicaenvase
							FROM dbo.plantadesp
							WHERE plde_codigo = :il_packing;
						
						IF li_aplicaenvase = 1 THEN
							IF Conexionexistencia() THEN
								dw_exideta.SetTransObject(sqlexi)
								dw_exiencab.SetTransObject(sqlexi)	
								dw_exismovtodetanulos.SetTransObject(sqlexi)
								dw_exidetaborra.SetTransObject(sqlexi)
								TriggerEvent("ue_despuesborrar2")
								TriggerEvent("ue_despuesguardar2")
								
								IF li_retorno = 2 THEN
									iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
									'Verificar Datos en Base de Existencia '+is_base,is_base,1,is_error,1,il_coderror,&
									is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
									
									li_retorno = 0	
								ELSE	
									iuo_grabatablabitacora.actualizaestado(ls_numero,ld_fecha,This.Title)
									
								END IF
								DISCONNECT USING sqlexi;
								
							ELSE
								iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
								'Verificar Conexión a Base '+is_base,is_base,1,'No Se Pudo Conectar el Sistema a Base de Datos de Existencia',1,il_coderror,&
								is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Con problemas, Error en Conexión con Base de Existencia '+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
								MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
							END IF
						ELSE
							MessageBox("Cuenta Corriente Envases", "Packing "+String(il_packing)+" NO Tiene Marca de Control de Envases a Packing Origen, NO Generá Movimiento en Existencia.",Exclamation!, OK!)	
						END IF
					END IF	
				ELSE
					iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This. Title,'','','',&
					'Verificar Conexión a Base '+is_base,is_base,1,'No Se Pudo Conectar el Sistema a Base de Datos de Existencia',1,il_coderror,&
					is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Con problemas, Error en Conexión con Base de Existencia '+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
					MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
				END IF
			END IF	
		END IF	
	END IF	
	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

IF Message.DoubleParm = -1 THEN RETURN

IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumFruta = 0
	il_NumEnva	= 0
END IF

IF dw_2.Object.mfge_estmov[1]	=	3 THEN
	ActualizaKilosLote()
END IF
end event

event ue_modifica_detalle;Integer	li_tabpage
Boolean	lb_estado

str_mant_envases	lstr_mant

li_tabpage			=	tab_1.SelectedTab

istr_mant.agrega	= 	False
istr_mant.borra	= 	False

lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1
		IF ib_Destare THEN
			istr_mant.Solo_Consulta	=	True
		END IF
		
		IF dw_3.RowCount() > 0 THEN
			istr_mant.dw	=	dw_3
			istr_mant.dw2	=	dw_6
			
			istr_mant.Argumento[7]	=	String(dw_3.Object.lote_codigo[il_fila])
			
			dw_spro_bins.SetFilter("lote_codigo = " + istr_mant.Argumento[7])
			dw_spro_bins.Filter()
			
			
			OpenWithParm(iw_mantencion_1, istr_mant)
			
			dw_spro_bins.SetFilter('')
			dw_spro_bins.Filter()
			IF NOT gstr_paramplanta.bultobins THEN
				AsignaPeso()
			END IF
			
		END IF
		istr_mant.Solo_Consulta	=	lb_estado
		CargaEnvases()

	CASE 2	
		IF ib_Destare THEN
			lstr_mant.Solo_Consulta	=	True
		END IF
		IF dw_5.RowCount() > 0 THEN
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'1'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		END IF
		lstr_mant.Solo_Consulta	=	lb_estado
		
	CASE 3				
		IF dw_7.RowCount() > 0 THEN
			lstr_mant.dw	=	dw_7
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'2'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		END IF
		
END CHOOSE

dw_6.SetFilter("")
dw_6.Filter()

captura_totales()
end event

event ue_nuevo;Long			ll_modif
Time 			lt_hora
str_pesaje	lstr_pesaje
str_puertacomm  lstr_puertacomm

is_rut		=	''
is_rutprod	=	''

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
			ll_modif	+=	dw_6.GetNextModified(0, Primary!)
			ll_modif	+=	dw_5.GetNextModified(0, Primary!)
			ll_modif	+=	dw_7.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 and ll_modif > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_3.Reset()
dw_6.Reset()
dw_4.Reset()
dw_5.Reset()
dw_7.Reset()
dw_9.Reset()
dw_spro_bins.Reset()

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()


pb_eli_det.Enabled				=	False
pb_ins_det.Enabled				=	False
pb_grabar.Enabled					=	False
pb_eliminar.Enabled				=	False
pb_imprimir.Enabled				=	False
dw_2.Enabled						=	True

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	1
dw_2.Object.espe_codigo[1]	=	gstr_ParamPlanta.CodigoEspecie
dw_2.Object.clie_codigo[1]		=	gi_CodExport

set_tarjas(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1])

idt_FechaSistema					=	F_FechaHora()

lt_hora								=	Time(Mid(String(idt_FechaSistema, 'dd/mm/yyyy hh:mm:ss'),12,8))

dw_2.Object.mfge_fecmov[1]		=	Date(String(idt_FechaSistema,'dd/mm/yyyy'))
dw_2.Object.refg_horaen[1]		=	lt_hora

dw_2.Object.destare.Visible	=	False
dw_2.Object.destare.Text		=	'Salida'
cb_guia.Enabled					=	False
ib_Salida								=	False
ib_Destare							=	False

istr_Mant.Argumento[3]			=	''
istr_mant.Argumento[5]			=	String(dw_2.Object.espe_codigo[1])
istr_mant.Argumento[7]			=	'0'

//IF iuo_especie.existe(gstr_ParamPlanta.CodigoEspecie,TRUE,SQLCA,Integer(istr_mant.argumento[10])) THEN
IF iuo_especie.existe(gstr_ParamPlanta.CodigoEspecie,TRUE,SQLCA) THEN
	IF iuo_especie.kildec = 1 THEN
		ii_kildec = 2
	ELSE
		ii_kildec = 0
	END IF
END IF	
		
tab_1.tp_1.Enabled	=	False
tab_1.tp_2.Enabled	=	False
tab_1.tp_3.Enabled	=	False
pb_ins_det.Enabled	=	False

tab_1.SelectTab(1)
HabilitaEncab(True)

dw_2.SetColumn("mfge_numero")
dw_2.SetFocus()

//Inicializa Estructura de Pesajes
wstr_pesaje				=	lstr_pesaje

CargaEnvases()
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer	li_tabpage
Boolean	lb_estado, lb_Salir

str_mant_envases	lstr_mant

li_tabpage			=	tab_1.SelectedTab

istr_mant.Borra	=	False
istr_mant.Agrega	=	True
	
lb_estado			=	istr_mant.Solo_Consulta

If li_tabpage <> 1 Then
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra						=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	ProductoresLotes(lstr_mant.productores)
End If

Choose Case li_tabpage
	Case 1
		If ib_Destare Then
			istr_mant.Solo_Consulta	=	True
		End If

		If Integer(istr_mant.argumento[7]) = 0 Then
			If iuo_Correlativo.Obtiene_Correlativo(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[5]), True, SQLCA) Then
				istr_mant.argumento[7]			=	String(iuo_Correlativo.Correl_LoteFG)
			Else
				lb_Salir	=	True
			End If
		End If
		
		If Not lb_Salir Then
			istr_mant.dw	=	dw_3
			istr_mant.dw2	=	dw_6
			istr_Mant.Argumento[8] = String(dw_2.Object.mfge_fecmov[1])
			
			OpenWithParm(iw_mantencion_1, istr_mant)
			istr_mant.Solo_Consulta	=	lb_estado
			
			If NOT gstr_paramplanta.bultobins Then
				AsignaPeso()
			End If
		End If
	
		If NOT istr_mant.Solo_Consulta Then
			Captura_totales()
			CargaEnvases()
		End If

	Case 2
		If ib_Destare Then
			lstr_mant.Solo_Consulta	=	True
		End If
		
		lstr_mant.dw	=	dw_5
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		lstr_mant.Solo_Consulta	=	lb_estado

	Case 3
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'2'
		OpenWithParm(iw_mantencion_2, lstr_mant)
End Choose

If dw_3.RowCount() > 0 AND dw_5.RowCount() > 0 AND NOT ib_Salida Then HabilitaEncab(False)

If dw_3.RowCount() > 0 AND dw_5.RowCount() > 0 AND NOT ib_Salida Then
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
	pb_eli_det.Enabled	=	True
End If

Choose Case li_tabpage 
	Case 1
		If Not lb_Salir Then
			istr_mant	=	Message.PowerObjectParm
			
			dw_3.SetRow(il_Fila)
			dw_3.SelectRow(il_Fila, True)
		End If
	Case 2
		
		dw_5.SetRow(il_Fila)
		dw_5.SelectRow(il_Fila, True)
	Case 3
		
		dw_7.SetRow(il_Fila)
		dw_7.SelectRow(il_Fila, True)
End Choose

dw_6.SetFilter("")
dw_6.Filter()


end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_env
Boolean lb_cargo = TRUE
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
										 Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[10]))

	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila_e > 0 Then
		is_rut = dw_2.Object.mfge_rutcho[1]
		
		Tab_1.TP_1.Enabled		=	True
		Tab_1.TP_2.Enabled		=	True
		
		If dw_2.Object.mfge_estmov[1]	=	3 Then
			tab_1.tp_3.Enabled			=	True
			istr_mant.Solo_Consulta		=	True
			dw_2.Enabled					=	False
		Else
			dw_2.Object.destare.Visible	=	True
		End If
		
		istr_mant.Argumento[5]	=	String(dw_2.Object.espe_codigo[1])
		
		dw_2.SetRedraw(True)
		
//		If Not gb_RecepcionDeProceso Then
//			iuo_Camion.Existe(1, dw_2.Object.cami_patent[1], True, sqlca)
//		End If
		
		//HabilitaEncab(False)
		
		ll_fila_env	=	dw_4.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),Long(istr_mant.argumento[3]),1, Integer(istr_mant.argumento[10]))
		DO
			dw_1.GetChild("lote_espcod", idwc_especiedet1)
			idwc_especiedet1.SetTransObject(sqlca)
			If idwc_especiedet1.Retrieve() = 0 Then
				MessageBox("Atención","Falta Registrar Especies")
				idwc_especiedet1.InsertRow(0)
			Else
				idwc_especiedet1.SetSort("espe_nombre A")
				idwc_especiedet1.Sort()
			End If
			
			dw_spro_bins.GetChild("fgmb_tibapa",idwc_bins)
			idwc_bins.SetTransObject(sqlca)
			idwc_bins.Retrieve(Integer(istr_mant.Argumento[10]),Integer(istr_mant.Argumento[1]), 0)
			
			If dw_1.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[10])) = -1 OR &
				dw_3.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
								  Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[10])) = -1 OR &
				dw_6.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[10])) = -1 OR &
				dw_5.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),1,Integer(istr_mant.argumento[10])) = -1 OR &
				dw_7.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),2,Integer(istr_mant.argumento[10])) = -1  OR &
				dw_spro_bins.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_9.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),Long(istr_mant.argumento[3])) = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Else
				idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1])
				
				dw_spro_bins.GetChild("fgmb_tibapa", idwc_tibapa)
				idwc_tibapa.SetTransObject(sqlca)
				idwc_tibapa.retrieve(Long(istr_mant.argumento[3]), Long(istr_mant.argumento[1]), 0)
				
				id_KilosEnv	=	dw_2.Object.refg_tkenen[1]		
				
				//Para determinar Fuera de Rango
				If dw_2.Object.mfge_estmov[1] = 3 Then Destare(False)
				
				pb_eliminar.Enabled  = Not istr_mant.Solo_Consulta
				pb_grabar.Enabled	= Not istr_mant.Solo_Consulta
				pb_ins_det.Enabled	= Not istr_mant.Solo_Consulta
				pb_imprimir.Enabled	= True
				pb_eli_det.Enabled	= Not istr_mant.Solo_Consulta
				dw_3.SetRow(1)
				dw_3.SelectRow(1, True)
				dw_3.SetFocus()
				
				If dw_3.RowCount() < 1 Then
					pb_ins_det.TriggerEvent(Clicked!)
				End If

			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1
							
If respuesta = 2 Then Close(This)
end event

event ue_antesguardar;
Date			ld_Fecha
Time			lt_hora
Boolean     lb_Actualiza_Fruta = FALSE, lb_Actualiza_Envase = FALSE
Integer 	 	li_TipoMovto, li_TipoMovtoEnva, li_Planta, li_pesaje, &
				li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo, li_Secuencia, &
				li_TipoEnvase, li_Envase, li_Lote_Ant, li_camara, li_Cliente
Long 			ll_Filas, ll_Total_Bultos, ll_Envases_Fruta, ll_Fila, ll_Fila_Busca, &
  				ll_Fila_d, ll_Primer_NumEnva, ll_Productor, ll_Numero
Decimal{3}	ld_Total_PesoEnv_Sal, ld_KBS_Camion, ld_KBS_Carro

Message.DoubleParm = 0

li_Planta					=	Integer(istr_mant.Argumento[1])
li_TipoMovto			=	Integer(istr_mant.Argumento[2])
li_TipoMovtoenva  		=  41
li_Cliente					=	Integer(istr_mant.Argumento[10])
 
ll_Filas = dw_1.RowCount()

IF dw_3.RowCount() > 0 THEN
	IF dw_3.Object.total_bultos[dw_3.RowCount()] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	ELSE
		ll_Total_Bultos	=	dw_3.Object.total_bultos[dw_3.RowCount()]
	END IF
END IF

IF dw_5.RowCount() > 0 THEN
	IF dw_5.Object.total_pesoenv[dw_5.RowCount()] >= 100000 THEN
		Messagebox("Error de Consistencia","El peso de los envases supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	END IF
	
	IF dw_5.Object.total_envases[dw_5.RowCount()] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de envases supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	END IF
END IF

IF dw_5.RowCount() > 0 THEN
	ll_Envases_Fruta	=	dw_5.Object.tot_bultos_fruta[dw_5.RowCount()]
END IF

IF NOT Chequea_EnvasesProductor() THEN
	Message.DoubleParm = -1
	RETURN
END IF

//IF NOT Chequea_BinsContraEnvases() THEN
//	Message.DoubleParm = -1
//	RETURN
//END IF

IF ib_Salida THEN
	IF Isnull(dw_2.Object.refg_tkbsal[1]) OR dw_2.Object.refg_tkbsal[1] = 0 THEN
		IF	MessageBox("Atención","No se ha registrado la Tara de Salida del Camión."+&
						" Desea Registrarla ?",Question!,YesNo!) = 1 THEN
			Message.DoubleParm = -1
			RETURN
		END IF
	END IF
	
	ll_Fila	=	dw_7.RowCount()

	IF ll_Fila > 0 THEN
		dw_7.GroupCalc()
		ld_Total_PesoEnv_Sal	=	dw_7.Object.total_pesoenv[ll_Fila]
	END IF

	ld_KBS_Camion	=	dw_2.Object.refg_tkbsal[1]
	ld_KBS_Carro	=	dw_2.Object.refg_tkbsac[1]

	IF ld_KBS_Camion + ld_KBS_Carro > 0 AND &
		ld_KBS_Camion + ld_KBS_Carro < ld_Total_PesoEnv_Sal THEN
		MessageBox("Atención","Kilos de Salida no pueden ser menor a los Envases de Salida")
		Message.DoubleParm = -1
		RETURN
	END IF
	
 	IF dw_2.Object.mfge_tpneto[1] >= 200000 THEN
		MessageBox("Atención","Los valores de la tara superan el calculo del Peso Neto permitido")
		Message.DoubleParm = -1
		RETURN
	END IF

	IF ib_Destare THEN
		//Pasa a Definitiva
		dw_2.Object.mfge_estmov[1]	=	3
		cb_guia.Enabled				=	True	
	ELSE
		MessageBox("Atención","Aún no realiza el destare, no puede grabar")
		Message.DoubleParm = -1
		RETURN
	END IF
	
END IF

IF dw_9.RowCount() <> dw_2.Object.mfge_totbul[1] AND (gstr_paramplanta.palletdebins OR  gstr_paramplanta.binsabins) THEN
	MessageBox("Atención","La cantidad de pesajes no corresponde a la cantidad de Bultos Recepcionados.")
	Message.DoubleParm = -1
	RETURN
END IF

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

IF dw_3.GetItemStatus(1, 0, Primary!) = NewModified! THEN
  
  	IF il_NumEnva=0 THEN
		iuo_TipoMovtoEnva.bloqueacorrel()	
		il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(4,li_TipoMovtoEnva,li_Planta) 
	
		IF il_NumEnva = 0 OR IsNull(il_NumEnva) THEN
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			RETURN
		ELSE
			lb_Actualiza_Envase = TRUE
			ll_Primer_NumEnva   = il_NumEnva
		END IF
	ELSE
		ll_Primer_NumEnva   = il_NumEnva
	END IF
	
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		IF il_NumFruta=0 THEN
			iuo_TipoMovtoFruta.bloqueacorrel()
			il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(1,li_TipoMovto,li_Planta) 
		
			IF il_NumFruta = 0 OR IsNull(il_NumFruta) THEN
				Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
				Message.DoubleParm = -1
				RETURN
			ELSE
				lb_Actualiza_Fruta = TRUE	
			END IF
		END IF
		dw_2.Object.mfge_numero[1]	=	il_NumFruta
	ELSE
		il_NumFruta				=	dw_2.Object.mfge_numero[1]
		lb_Actualiza_Fruta	=	False
   END IF

	Determina_ProductoresEnvase(li_TipoMovtoEnva)
	
	//Resetea Movto de Envase en caso de errores previos en la grabación.
	dw_4.Reset()
	
	FOR ll_Productor = 1 TO UpperBound(wstr_Prod_Enva.Productor)
		ll_Fila	=	dw_4.InsertRow(0)
		
		dw_4.Object.plde_codigo[ll_Fila]		=		dw_2.Object.plde_codigo[1]
		dw_4.Object.tpmv_codigo[ll_Fila]		=		li_TipoMovtoEnva
		dw_4.Object.meen_numero[ll_Fila]		=		il_NumEnva
		dw_4.Object.tpmv_codrec[ll_Fila]		=		dw_2.Object.tpmv_codigo[1]
		dw_4.Object.mfge_numero[ll_Fila]		=		dw_2.Object.mfge_numero[1]	
		dw_4.Object.meen_modulo[ll_Fila]		=		1
		dw_4.Object.plde_coorde[ll_Fila]		=		dw_2.Object.plde_coorde[1]
		dw_4.Object.prod_codigo[ll_Fila]		=		Long(wstr_Prod_Enva.Productor[ll_Productor])
     	dw_4.Object.clie_codigo[ll_Fila]		=		li_Cliente
		dw_4.Object.meen_guisii[ll_Fila]		=		wstr_Prod_Enva.GuiaSII[ll_Productor]
		dw_4.Object.meen_fecmov[ll_Fila]		=		dw_2.Object.mfge_fecmov[1]
		dw_4.Object.tran_codigo[ll_Fila]		=		dw_2.Object.tran_codigo[1]
		dw_4.Object.cami_clasifi[ll_Fila]	=		dw_2.Object.cami_clasifi[1]
		dw_4.Object.cami_patent[ll_Fila]		=		dw_2.Object.cami_patent[1]
		dw_4.Object.cami_patcar[ll_Fila]		=		dw_2.Object.cami_patcar[1]
		dw_4.Object.meen_rutcho[ll_Fila]		=		dw_2.Object.mfge_rutcho[1]
		dw_4.Object.meen_chofer[ll_Fila]		=		dw_2.Object.mfge_chofer[1]
		il_NumEnva++
	NEXT
	//Descuenta último Correlativo Envases acumulado.
	il_NumEnva --
	
	//Preguntar el Momento de Actualización
	IF lb_Actualiza_Fruta  THEN iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
	IF lb_Actualiza_Envase THEN iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
	
//	il_NumEnva = ll_Primer_NumEnva	
ELSE
	il_NumFruta	=	dw_2.Object.mfge_numero[1]
END IF

istr_mant.Argumento[3]	=	String(il_NumFruta)

SELECT	IsNull(Max(mfgd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM dbo.spro_movtofrutagrandeta
	WHERE	plde_codigo		=	:li_Planta
	  AND	tpmv_codigo	=	:li_TipoMovto
	  AND	mfge_numero	=	:il_NumFruta
	  AND clie_codigo =  :li_Cliente;
	
ll_filas = dw_1.RowCount()	

FOR ll_Fila = 1 TO dw_6.RowCount()
	
	li_Lote_pltcod											=		dw_6.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod											=		dw_6.Object.lote_espcod[ll_Fila]
	li_Lote_codigo											=		dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase											=		dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase												=		dw_6.Object.enva_codigo[ll_Fila]

	IF li_Lote_Codigo <> li_Lote_Ant THEN
		ll_Fila_Busca	=	dw_3.Find("lote_pltcod 	= "	+	String(li_Lote_pltcod)+" and "+&
											 "lote_espcod 	= "	+	String(li_Lote_espcod)+" and "+&
											 "lote_codigo 	= "	+	String(li_Lote_codigo),1,dw_3.RowCount())
		IF ll_Fila_Busca > 0 THEN
			dw_3.Object.fgcc_fecrec[ll_Fila_Busca]	=		dw_2.Object.mfge_fecmov[1]
			li_Camara										=		dw_3.Object.cama_codigo[ll_Fila_Busca]
		END IF
		li_Lote_Ant											=		li_Lote_codigo
	END IF
	
	ll_Fila_d	=	dw_1.Find("lote_pltcod 	= "	+	String(li_Lote_pltcod)+" and "+&
									 "clie_codigo 	= "	+	String(li_cliente)+" and "+&
									 "lote_espcod 	= "	+	String(li_Lote_espcod)+" and "+&
									 "lote_codigo 	= "	+	String(li_Lote_codigo)+" and "+&
									 "enva_tipoen 	= "	+	String(li_TipoEnvase)+" and "+&
									 "enva_codigo 	= "	+	String(li_Envase),1,dw_1.RowCount())
	IF ll_Fila_d = 0 THEN
		ll_Fila_d	=	dw_1.InsertRow(0)
		dw_1.Object.plde_codigo[ll_Fila_d]	=	li_Planta
		dw_1.Object.tpmv_codigo[ll_Fila_d]	=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfge_numero[ll_Fila_d]	=	dw_2.Object.mfge_numero[1]
		dw_1.Object.mfgd_secuen[ll_Fila_d]	=	li_Secuencia
		li_Secuencia ++
	END IF

	dw_1.Object.plde_coorde[ll_Fila_d]		=	li_Planta
	dw_1.Object.cama_codigo[ll_Fila_d]		=	li_Camara
	dw_1.Object.lote_pltcod[ll_Fila_d]		=	li_Lote_pltcod
	dw_1.Object.lote_espcod[ll_Fila_d]		=	li_Lote_espcod
	dw_1.Object.lote_codigo[ll_Fila_d]		=	li_Lote_codigo
	dw_1.Object.enva_tipoen[ll_Fila_d]		=	li_TipoEnvase
	dw_1.Object.enva_codigo[ll_Fila_d]		=	li_Envase
	dw_1.Object.mfgd_bulent[ll_Fila_d]		=	dw_6.Object.lotd_totbul[ll_Fila]
	dw_1.Object.mfgd_kgnent[ll_Fila_d]		=	dw_6.Object.lotd_totnet[ll_Fila]
	dw_1.Object.clie_codigo[ll_Fila_d]		=	li_Cliente	
NEXT

//Elimina Filas de Detalle de Movimiento que fueron eliminadas de Lotes
ll_Fila	= 1

DO WHILE ll_Fila <= dw_1.RowCount()
	
	li_Lote_pltcod	=	dw_1.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod	=	dw_1.Object.lote_espcod[ll_Fila]
	li_Lote_codigo	=	dw_1.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_1.Object.enva_tipoen[ll_Fila]
	li_Envase			=	dw_1.Object.enva_codigo[ll_Fila]

	ll_Fila_d	=	dw_6.Find("lote_pltcod 	= "	+	String(li_Lote_pltcod)+" and "+&
	                         "lote_espcod 	= "	+	String(li_Lote_espcod)+" and "+&
									 "lote_codigo 	= "	+	String(li_Lote_codigo)+" and "+&
									 "enva_tipoen 	= "	+	String(li_TipoEnvase)+" and "+&
									 "enva_codigo 	= "	+	String(li_Envase),1,dw_6.RowCount())

	IF ll_Fila_d = 0 THEN
		dw_1.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP

FOR ll_Fila = 1 TO dw_5.RowCount()
	
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Productor	=	dw_5.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_4.Find("plde_codigo	= "	+	String(li_Planta) 			+ " and " + &
											 "tpmv_codigo 	= "	+	String(li_TipoMovtoEnva) 	+ " and " + &
											 "clie_codigo 	= "	+	String(li_Cliente) 			+ " and " + &											 
											 "prod_codigo 	= "	+	String(ll_Productor),1,dw_4.RowCount())
		
		IF ll_Fila_Busca > 0 THEN
												 
			dw_5.Object.plde_codigo[ll_Fila]		=	dw_4.Object.plde_codigo[ll_Fila_Busca]
			dw_5.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva
			dw_5.Object.meen_numero[ll_Fila]		=	dw_4.Object.meen_numero[ll_Fila_Busca]
			dw_5.Object.clie_codigo[ll_Fila]		=	li_Cliente
		END IF
	END IF
NEXT

FOR ll_Fila = 1 TO dw_7.RowCount()
	
	IF dw_7.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Productor	=	dw_7.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_4.Find("plde_codigo 	= "			+	String(li_Planta)+" and " + &
											 "tpmv_codigo 	= 41 and " 	+ 	&
											 "clie_codigo 	= "			+	String(li_Cliente) + " and " + &											 
											 "prod_codigo 	= "			+	String(ll_Productor),1,dw_4.RowCount())
		
		IF ll_Fila_Busca > 0 THEN
												 
			dw_7.Object.plde_codigo[ll_Fila]		=	dw_4.Object.plde_codigo[ll_Fila_Busca]
			dw_7.Object.tpmv_codigo[ll_Fila]		=	dw_4.Object.tpmv_codigo[ll_Fila_Busca]
			dw_7.Object.meen_numero[ll_Fila]		=	dw_4.Object.meen_numero[ll_Fila_Busca]
			dw_7.Object.clie_codigo[ll_Fila]		=	li_Cliente			
		END IF
	END IF
NEXT
/*Selecciona secuencia para tabla de pesajes*/

ll_numero	=	dw_2.Object.mfge_numero[1]
li_Pesaje		=	0

dw_9.SetSort("mfgp_nropes asc")
dw_9.Sort()
 
FOR ll_Fila = 1 TO dw_9.RowCount()
	IF dw_9.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_9.Object.clie_codigo[ll_fila] 	= 	dw_2.Object.clie_codigo[1]
		dw_9.Object.plde_codigo[ll_fila]	=	dw_2.Object.plde_codigo[1]
		dw_9.Object.tpmv_codigo[ll_fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_9.Object.mfge_numero[ll_fila]	=	dw_2.Object.mfge_numero[1]		
	END IF
NEXT

FOR ll_Fila = 1 TO dw_spro_bins.RowCount()
		dw_spro_bins.Object.mfge_numero[ll_fila]	=	dw_2.Object.mfge_numero[1]
//		dw_spro_bins.Object.cama_codigo[ll_fila]	=	0
NEXT

//SETEA CLIENTE = CLIE_CODIGO SELECCIONADO
//Repara falla para proceso 3ros.
FOR ll_fila = 1 TO dw_1.Rowcount()
	dw_1.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_3.Rowcount()
	dw_3.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_4.Rowcount()
	dw_4.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_5.Rowcount()
	dw_5.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_7.Rowcount()
	dw_7.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

IF gb_RecepcionDeProceso THEN
	dw_2.Object.defg_tipdoc[1]		=	8
END IF

//AsignaPeso()
end event

event ue_seleccion;call super::ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_ParamPlanta.CodigoPlanta)
lstr_busq.argum[2] = '1'								// Movimiento de Recepción
lstr_busq.argum[3] = ''								// Cualquier estado
lstr_busq.argum[4] = String(idt_FechaSistema)   // Desde Fecha de Inicio Ducha
lstr_busq.argum[5] = istr_mant.argumento[5]     // Especie
lstr_busq.argum[10] = istr_mant.argumento[10]   // Codigo Cliente

OpenWithParm(w_busc_spro_movtofrutagranenca_recepcion, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.Argumento[2]	=	lstr_busq.argum[2]
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	istr_mant.argumento[10]	=	lstr_busq.argum[10]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_imprimir;Long	ll_modIf
Date	ld_FechaRecepcion

If Not istr_mant.Solo_Consulta Then
	CHOOSE CASE wf_modIfica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modIf	=	dw_1.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_2.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_3.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_6.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_5.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_7.GetNextModIfied(0, Primary!)
		
			If dw_3.RowCount() > 0 and ll_modIf > 0 Then
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información antes de Imprimir ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						If message.DoubleParm = -1 Then ib_ok = False
					CASE 3
						ib_ok	= False
						Return
				End CHOOSE
			End If
	End CHOOSE
End If

If Not ib_ok Then Return

SetPointer(HourGlass!)

Long		fila
Integer li_estado, li_Kilos

istr_info.titulo	= "GUIA DE RECEPCION FRUTA GRANEL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

li_estado					=	dw_2.object.mfge_estmov[1]
ld_FechaRecepcion	=	dw_2.object.mfge_fecmov[1]

If li_estado=1 Then
   vinf.dw_1.DataObject = "dw_info_guia_recepcion_Trans_cereza"
Else
	vinf.dw_1.DataObject = "dw_info_guia_recepcion_Definitiva"
	li_Kilos	=	MessageBox("Emisión Definitiva","Desea emitir Guía de Recepción con Kilos",Question!,YesNo!,1)
	If li_Kilos = 2 Then li_Kilos = 0
End If

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Long(istr_mant.Argumento[1]), Long(istr_mant.Argumento[2]), Long(istr_mant.Argumento[3]),&
								  Integer(istr_mant.argumento[10]), ld_FechaRecepcion, ld_FechaRecepcion)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy('DataWindow.Print.Preview = Yes')
	vinf.dw_1.ModIfy('DataWindow.Print.Preview.Zoom = 75')
	vinf.Visible	= True
	vinf.Enabled	= True
End If

SetPointer(Arrow!)
end event

event closequery;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_3.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.triggerevent("ue_guardar")
						IF message.doubleparm = -1 THEN Message.ReturnValue = 1
						RETURN
					CASE 3
						Message.ReturnValue = 1
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF
end event

event ue_validaborrar();IF dw_2.Object.mfge_estmov[1] > 1 THEN
	MessageBox("Atención","No puede ser eliminado un movimiento en estado definitivo")
	Message.DoubleParm = -1
	RETURN
ELSE
	IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
		Message.DoubleParm = 1
	ELSE
		Message.DoubleParm = -1
	END IF
END IF

RETURN 

end event

event mousemove;call super::mousemove;//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 0
integer y = 824
integer width = 3634
integer height = 200
boolean titlebar = false
string title = "Detalle de Movimientos"
string dataobject = "dw_mues_movtofrutagraneldeta_despacho"
boolean hscrollbar = false
boolean vscrollbar = false
boolean border = true
boolean livescroll = false
borderstyle borderstyle = stylebox!
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_recepcereza
integer x = 105
integer y = 16
integer width = 3273
integer height = 932
string dataobject = "dw_mant_movtofrutagranenca"
boolean livescroll = true
boolean righttoleft = true
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna
Date		ld_Fecha
Time		lt_Hora
 
ls_Columna = dwo.Name
SetNull(ls_Nula)
 
CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		ELSE
			dw_2.GetChild("espe_codigo", idwc_especie)
			idwc_especie.SetTransObject(sqlca)
			idwc_especie.Retrieve(integer(data))
			istr_mant.Argumento[10] = data
			set_tarjas(Integer(data), This.Object.plde_codigo[1])
		END IF	
	
	CASE "mfge_numero"
		IF NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, 1, Long(data),Integer(istr_mant.argumento[10])) THEN
			This.SetItem(row,"mfge_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			istr_mant.Argumento[3] = Data
		END IF

	CASE "mfge_fecmov"
		ld_Fecha	=	Date(Mid(String(date(data),'dd/mm/yyyy'),1,10))
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(ld_Fecha) THEN
			This.SetItem(row,"mfge_fecmov",Date(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF	

	CASE "espe_codigo"
		IF Not manbin_especie(This.Object.plde_codigo[row], Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSEIF Not iuo_Especie.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSEIF NOT iuo_Especie.ExisteCorrelativoLote(gstr_ParamPlanta.CodigoPlanta,Integer(Data),True,SQLCA) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			istr_mant.argumento[5]	=	data
			istr_mant.argumento[7]	=	'0'
			IF iuo_especie.kildec = 1 THEN
				ii_kildec = 2
			ELSE
				ii_kildec = 0
			END If
			
			idwc_Variedad.SetTransObject(SQLCA)
			idwc_Variedad.Retrieve(Integer(data))
		END IF

	CASE "tran_codigo"
		IF Not iuo_Transport.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		END IF

	CASE "cami_patent"
		IF data <> "" AND Not iuo_Camion.Existe(1, Data, True, sqlca) THEN
			This.SetItem(row, ls_Columna, ls_Nula)
			
			RETURN 1
		ELSE
			This.Object.cami_patcar[row]	=	iuo_Camion.PateCarro
			This.Object.mfge_rutcho[row]	=	iuo_Camion.RutChofer
			is_rut = iuo_Camion.RutChofer
			This.Object.mfge_chofer[row]	=	iuo_Camion.Chofer
		END IF

	CASE "mfge_rutcho"
		
		is_rut = F_verrut(data, True)
		IF is_rut = "" THEN
			dw_2.SetItem(row, "mfge_rutcho", ls_Nula)
			dw_2.SetItem(row, "mfge_chofer", ls_Nula)
			RETURN 1
		ELSE
			IF NoExisteChofer(Data) THEN
				dw_2.SetItem(row, "mfge_rutcho", ls_Nula)
				dw_2.SetItem(row, "mfge_chofer", ls_Nula)
				RETURN 1
			ELSE	
				dw_2.SetItem(row, "mfge_chofer",is_chofer)
			END IF
		END IF	
		
	CASE "refg_tkbent", "refg_tkbenc", "refg_tkbsal", "refg_tkbsac"
		IF Not This.uf_validate(row) THEN
			This.SetItem(row,ls_Columna,Dec(ls_Nula))
			RETURN 1
		ELSE
			captura_Totales()
		END IF
		
	CASE "mfge_totbul"
		IF Integer(data) > 9999 OR Integer(data) < 0 THEN
			This.SetItem(row,"mfge_totbul",Integer(ls_Nula))
			RETURN 1
		END IF

	CASE "fecha_sal"
		ld_Fecha	=	Date(String(Mid(data,1,10),'dd/mm/yyyy'))
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(date(ld_Fecha)) OR &
			ld_Fecha < This.Object.mfge_fecmov[row] THEN
			MessageBox("Atención","Fecha de Salida no puede ser anterior a Fecha de Entrada")
			This.SetItem(row,"fecha_sal",SetNull(ld_Fecha))
			RETURN 1
		END IF
		
		
	CASE "refg_horasa"
		lt_Hora	=	Time(This.Object.fecha_sal[Row])
	
		IF Time(data) < This.Object.refg_horaen[row] THEN
			MessageBox("Atención","Hora de Salida no puede ser anterior a Hora de Entrada")
			This.SetItem(row,"refg_horasa",SetNull(lt_Hora))
			RETURN 1
		END IF
		
	CASE "defg_docrel"
		IF Data <> "" and Data <> "0" THEN
			IF Not Existedocproceso(gstr_ParamPlanta.CodigoPlanta,Integer(Data)) THEN
				dw_2.SetItem(1, 'defg_docrel', integer(ls_Nula))
				RETURN 1
			ELSE
				dw_2.Object.defg_docrel[row]		=	Integer(Data)
			END IF
			dw_2.AcceptText()
		END IF
		
END CHOOSE

IF ls_Columna <> 'mfge_numero' THEN
	HabilitaIngreso(ls_Columna)
	
	IF ib_Salida THEN
		HabilitaGrabacion(ls_Columna)
	END IF
END IF
end event

event dw_2::buttonclicked;call super::buttonclicked;Long 				ll_Fila
String 			ls_nula
decimal			ld_peso
Str_busqueda	lstr_busq

SetNull(ls_nula)

CHOOSE CASE dwo.Name
	CASE "buscacamion"
		buscacamion()
		iuo_Camion.Existe(1, This.Object.cami_patent[row], True, sqlca)

	CASE "destare"
		IF Not ib_Salida THEN
			HabilitaSalida()
		ELSE
			Destare(True)
			ib_graba_destare = true
			IF gstr_paramplanta.bultobins THEN
				UpdateMovtoGranPesa()
			END IF
		END IF

	CASE "camion"
		IF ib_Salida THEN
			IF Isnull(iuo_Camion.TaraCamion) THEN
				This.Object.refg_tkbsal[row]	=	0
			ELSE
				This.Object.refg_tkbsal[row]	=	iuo_Camion.TaraCamion
				captura_totales()
			END IF
		END IF

	CASE "carro"
		IF ib_Salida THEN
			IF Isnull(iuo_Camion.TaraCarro) THEN
				This.Object.refg_tkbsac[row]	=	0
			ELSE
				This.Object.refg_tkbsac[row]	=	iuo_Camion.TaraCarro
				captura_totales()
			END IF
		END IF

	CASE "romanacamion"
		IF gstr_paramplanta.bultobins THEN

			wstr_pesaje.puerta	=	istr_puertacomm
			OpenWithParm(w_pesaje_romana_camion,wstr_pesaje)

			ld_peso					=	Message.DoubleParm
			IF ld_peso > 0 THEN
				IF NOT ib_salida THEN
					This.Object.refg_tkbent[1]	=	ld_peso
				ELSE
					This.Object.refg_tkbsal[1]	=	ld_peso
				END IF
			END IF

		ELSE
//			IF NOT ib_salida THEN
//				wstr_pesaje.puerta	=	istr_puertacomm
//				wstr_pesaje.dw			=	dw_9
//
//				wstr_pesaje.argum[1]	=	istr_mant.argumento[1]
//				wstr_pesaje.argum[2]	=	istr_mant.argumento[2]
//				wstr_pesaje.argum[3]	=	istr_mant.argumento[3]
//				wstr_pesaje.argum[7]	=	"-1"
//				wstr_pesaje.argum[10]	=	istr_mant.argumento[10]
//
//				OpenWithParm(w_pesaje_romana,wstr_pesaje)
//
//				wstr_pesaje	=	Message.PowerObjectParm
//
//				This.Object.refg_tkbent[1]	=	0
//
//				captura_Totales()
//			ELSE
//				wstr_pesaje.puerta	=	istr_puertacomm
//				wstr_pesaje.dw			=	dw_9
//				wstr_pesaje.puerta.pesajebins	=	0
//
//				OpenWithParm(w_pesaje_romana,wstr_pesaje)
//
//				wstr_pesaje	=	Message.PowerObjectParm
//
//				This.Object.refg_tkbsal[1]	=	0
//
//				captura_Totales()
//
//			END IF
//			AsignaPeso()

		END IF

	CASE "romanacarro"
		IF gstr_paramplanta.bultobins THEN

			wstr_pesaje.puerta	=	istr_puertacomm
			OpenWithParm(w_pesaje_romana_camion,wstr_pesaje)

			ld_peso					=	Message.DoubleParm
			IF ld_peso > 0 THEN
				IF NOT ib_salida THEN
					This.Object.refg_tkbenc[1]	=	ld_peso
				ELSE
					This.Object.refg_tkbsac[1]	=	ld_peso
				END IF
			END IF

		ELSE
//			IF NOT ib_salida THEN
//				wstr_pesajecarro.puerta	=	istr_puertacomm
//				wstr_pesajecarro.dw		=	dw_9
//				OpenWithParm(w_pesaje_romana,wstr_pesajecarro)
//
//				wstr_pesajecarro	=	Message.PowerObjectParm
//
//				This.Object.refg_tkbenc[1]	=	0
//				
//				IF dw_9.RowCount() > 0 THEN
//					FOR ll_Fila = 1 TO dw_9.RowCount()
//						IF dw_9.Object.mfgp_estado[ll_Fila] = 2 THEN
//							This.Object.refg_tkbenc[1]	=+	dw_9.Object.mfgp_pesore[ll_fila]
//						END IF
//					NEXT
//				END IF
//
//				captura_Totales()
//			ELSE
//				wstr_pesajecarro.puerta	=	istr_puertacomm
//				wstr_pesajecarro.dw		=	dw_9
//				wstr_pesajecarro.puerta.pesajebins	=	0
//
//				OpenWithParm(w_pesaje_romana,wstr_pesajecarro)
//
//				wstr_pesajecarro	=	Message.PowerObjectParm
//
//				This.Object.refg_tkbsac[1]	=	0
//
//				IF dw_9.RowCount() > 0 THEN
//					FOR ll_Fila = 1 TO dw_9.RowCount()
//						IF dw_9.Object.mfgp_estado[ll_Fila] = 2 THEN
//							This.Object.refg_tkbsac[1]	=+	dw_9.Object.mfgp_pesore[ll_fila]
//						END IF
//					NEXT
//				END IF	
//				captura_Totales()
//			
//			END IF

		END IF

	CASE "ordenproceso"
		lstr_busq.argum[1]	=	istr_mant.argumento[1]
		lstr_busq.argum[15]	=	"8"
		lstr_busq.argum[16]	=	istr_mant.Argumento[10]

		OpenWithParm(w_busc_spro_ordenproceso_traspaso, lstr_busq)
		lstr_busq	=	Message.PowerObjectParm

		IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
			IF Not Existedocproceso(gstr_ParamPlanta.CodigoPlanta,Integer(lstr_busq.argum[1])) THEN
				dw_2.SetItem(1, 'defg_docrel', integer(ls_Nula))
				RETURN 1
			ELSE
				dw_2.Object.defg_docrel[row]		=	Integer(lstr_busq.argum[1])
			END IF
			dw_2.AcceptText()
		END IF

END CHOOSE
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	This.Object.mfge_rutcho.Format = '@@@.@@@.@@@-@'
	
//	IF dwo.Name <> "mfge_rutcho" THEN
//		This.SetItem(1, "mfge_rutcho", is_rut)
//	END IF
END IF
end event

event dw_2::constructor;call super::constructor;This.uf_add_validation('refg_tkbsal >= 0 and refg_tkbsal <= 99999.999','Valor fuera de rango')
This.uf_add_validation('refg_tkbsac >= 0 and refg_tkbsac <= 99999.999','Valor fuera de rango')
This.uf_add_validation('refg_tkbent >= 0 and refg_tkbent <= 99999.999','Valor fuera de rango')
This.uf_add_validation('refg_tkbenc >= 0 and refg_tkbenc <= 99999.999','Valor fuera de rango')
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_recepcereza
integer x = 3721
integer y = 320
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_recepcereza
integer x = 3721
integer y = 572
integer height = 236
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_recepcereza
integer x = 3721
integer y = 612
boolean enabled = true
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_recepcereza
integer x = 3730
integer y = 952
end type

event pb_imprimir::clicked;call super::clicked;if dw_2.object.mfge_estmov[dw_2.getrow()] = 3 then
	Parent.TriggerEvent("ue_imprimir_tarja")
end if


end event

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_recepcereza
integer x = 3721
integer y = 1328
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_recepcereza
integer x = 3726
integer y = 1444
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_recepcereza
integer x = 3749
integer y = 1616
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_recepcereza
integer x = 3721
integer y = 68
end type

type cb_guia from commandbutton within w_maed_movtofrutagranel_recepcereza
integer x = 3721
integer y = 1188
integer width = 302
integer height = 88
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Guía SII"
end type

event clicked;Long		ll_Fila, ll_Fila_Busca,ll_Productor

str_mant	lstr_mant

Determina_ProductoresEnvase(61)

FOR ll_Fila = 1 TO UpperBound(wstr_Prod_Enva.Productor)
	ll_Productor	=	wstr_Prod_Enva.Productor[ll_Fila]
	
	ll_Fila_Busca	=	dw_4.Find("clie_codigo = "+istr_Mant.Argumento[10]+" and "+&
	                            "plde_codigo = "+istr_Mant.Argumento[1]+" and "+&
										 "tpmv_codigo = 41 and "+&
										 "prod_codigo = "+String(ll_Productor),1,dw_4.RowCount())
		
	IF ll_Fila_Busca > 0 THEN
												 
		lstr_Mant.Argumento[1]	=	String(dw_4.Object.plde_codigo[ll_Fila_Busca])
		lstr_Mant.Argumento[2]	=	String(dw_4.Object.tpmv_codigo[ll_Fila_Busca])
		lstr_Mant.Argumento[3]	=	String(dw_4.Object.meen_numero[ll_Fila_Busca])
		lstr_mant.Argumento[5]  =  String(dw_4.Object.clie_codigo[ll_Fila_Busca])
		
		OpenWithParm(w_emis_guia_despacho_envases, lstr_Mant)
	END IF
NEXT


end event

type dw_4 from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 814
integer y = 2012
integer width = 206
integer height = 164
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_movtoenvaenca_recepfruta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_spro_bins from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 1504
integer y = 1996
integer width = 283
integer height = 196
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle cajas"
string dataobject = "dw_spro_movtobins_cerezas"
borderstyle borderstyle = stylelowered!
end type

type dw_9 from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer width = 498
integer height = 280
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_pesaje_romana_cereza"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event sqlpreview;String	ls_SQL1, ls_SQL2, ls_SQL

//DebugBreak ( )

If SqlType = PreviewInsert! Then
	ls_SQL1 = Mid(SQLSyntax, 1, Pos(SQLSyntax, '(', 1) - 1)
	ls_SQL2 = Mid(SQLSyntax, Pos(SQLSyntax, '(', 1))
	
	ls_Sql =	ls_SQL1 +  ' with (tablock) ' + ls_SQL2
	
	This.SetSQLPreview(ls_Sql)
ENd If
end event

type dw_6 from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 69
integer y = 2020
integer width = 247
integer height = 168
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagradet_recepcion"
borderstyle borderstyle = stylelowered!
end type

type dw_exideta from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 768
integer y = 2272
integer width = 686
integer height = 400
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
		
	CASE primary!
		DwItemStatus Stat
		
		Stat	=	This.getitemstatus(Row, 0, Buffer)
		
		IF Stat = New! OR Stat = NewModified! THEN
			ls_Tipo	=	"Agregando"
		ELSE
			ls_Tipo	=	"Actualizando"
		END IF
		
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText
is_error				=  SqlErrText
il_coderror			=	SqlDbCode

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

//OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

type dw_exiencab from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer y = 2252
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
		
	CASE primary!
		DwItemStatus Stat
		
		Stat	=	This.getitemstatus(Row, 0, Buffer)
		
		IF Stat = New! OR Stat = NewModified! THEN
			ls_Tipo	=	"Agregando"
		ELSE
			ls_Tipo	=	"Actualizando"
		END IF
		
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText
is_error				=  SqlErrText
il_coderror			=	SqlDbCode

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

//OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 3031
integer y = 2280
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 1573
integer y = 2292
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_envases_comer from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 2299
integer y = 2284
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_detalle_envases_comercial"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_lotescategoria from datawindow within w_maed_movtofrutagranel_recepcereza
boolean visible = false
integer x = 361
integer y = 2036
integer width = 133
integer height = 144
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagranel_recepcion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ole_puerta from olecustomcontrol within w_maed_movtofrutagranel_recepcereza
event oncomm ( )
boolean visible = false
integer x = 3515
integer y = 36
integer width = 174
integer height = 152
integer taborder = 30
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_maed_movtofrutagranel_recepcereza.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type tab_1 from tab within w_maed_movtofrutagranel_recepcereza
integer x = 142
integer y = 972
integer width = 3360
integer height = 1044
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.tp_3=create tp_3
this.Control[]={this.tp_1,&
this.tp_2,&
this.tp_3}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
destroy(this.tp_3)
end on

type tp_1 from userobject within tab_1
string tag = "Detalle de Lotes Ingresados"
integer x = 18
integer y = 112
integer width = 3323
integer height = 916
boolean enabled = false
long backcolor = 16777215
string text = "Lotes Recepcionados"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "DatabaseProfile5!"
long picturemaskcolor = 536870912
dw_lotes dw_lotes
end type

on tp_1.create
this.dw_lotes=create dw_lotes
this.Control[]={this.dw_lotes}
end on

on tp_1.destroy
destroy(this.dw_lotes)
end on

type dw_lotes from uo_dw within tp_1
integer x = 18
integer y = 16
integer width = 3017
integer height = 872
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mues_spro_lotesfrutagranel_recepcion"
boolean hscrollbar = true
boolean livescroll = true
end type

event doubleclicked;call super::doubleclicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

w_maed_movtofrutagranel_recepcereza.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_recepcereza.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_recepcereza.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type tp_2 from userobject within tab_1
string tag = "Registro de Envases recepcionados con y sin fruta"
integer x = 18
integer y = 112
integer width = 3323
integer height = 916
boolean enabled = false
long backcolor = 16777215
string text = "Envases Recibidos"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "CreateRuntime!"
long picturemaskcolor = 536870912
dw_envrec dw_envrec
end type

on tp_2.create
this.dw_envrec=create dw_envrec
this.Control[]={this.dw_envrec}
end on

on tp_2.destroy
destroy(this.dw_envrec)
end on

type dw_envrec from uo_dw within tp_2
integer x = 27
integer y = 36
integer width = 3026
integer height = 840
integer taborder = 11
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
boolean hscrollbar = true
boolean livescroll = true
end type

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_recepcereza.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_recepcereza.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

type tp_3 from userobject within tab_1
string tag = "Registro de Envases que retira para cosecha"
integer x = 18
integer y = 112
integer width = 3323
integer height = 916
boolean enabled = false
long backcolor = 16777215
string text = "Envases Retirados"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Inherit!"
long picturemaskcolor = 536870912
dw_envdes dw_envdes
end type

on tp_3.create
this.dw_envdes=create dw_envdes
this.Control[]={this.dw_envdes}
end on

on tp_3.destroy
destroy(this.dw_envdes)
end on

type dw_envdes from uo_dw within tp_3
integer x = 27
integer y = 36
integer width = 3264
integer height = 840
integer taborder = 21
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_recepcion.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_recepcion.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Bw_maed_movtofrutagranel_recepcereza.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Bw_maed_movtofrutagranel_recepcereza.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
